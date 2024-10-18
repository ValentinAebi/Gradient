package gradcc.typechecking

import gradcc.*
import gradcc.asts.UniquelyNamedTerms
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*
import gradcc.lang.Keyword.SelfKw
import gradcc.prettyprinting.TermsPrettyprinter
import gradcc.typechecking.SubtypingRelation.*

import scala.collection.mutable


final class TypeCheckerPhase extends SimplePhase[Term, Map[Term, Type]]("Typechecker") {
  private val varCreator = SyntheticVarCreator()

  private val pp: Term => String = TermsPrettyprinter(UniquelyNamedTerms)

  override protected def runImpl(in: Term, reporter: Reporter): Map[Term, Type] = {
    val types: TermsTypes = mutable.Map.empty
    computeTypes(in)(using Ctx(Map.empty, types, reporter))
    types.flatMap {
      case (_, None) => None
      case (term, Some(tpe)) => Some(term -> tpe)
    }.toMap
  }

  private def computeTypes(t: Term)(using ctx: Ctx): Option[Type] = ctx.types.getOrElseUpdate(t, {
    // TODO pack and unpack
    // TODO double-check every typing rule
    import ctx.*
    val tpe = t match {
      case Identifier(id, position) =>
        // id must be found, o.w. the renaming phase stops the pipeline before it reaches this point
        ctx.varLookup(id)
      case Cap(position) =>
        throw AssertionError("unexpected type computation on the root capability")
      case Select(owner, field, position) =>
        computeTypes(owner).flatMap {
          case Type(RecordShape(selfRef, fields), captureSet) if fields.contains(mkField(field)) =>
            fields.get(mkField(field))
          case otherType => reportError(
            s"no '${mkField(field)}' field found in owner type $otherType", position)
        }
      case Box(boxed, position) =>
        computeTypes(boxed).map(BoxShape(_) ^ Set.empty)
      case abs@Abs(varIdent, varTypeTree, body, position) => {
        varTypeTree.captureSet.foreach(checkCaptureSet)
        val varId = varIdent.id
        val varType = mkType(varTypeTree)
        computeTypes(body)(using ctx.withNewBinding(varId, Some(varType))).map { bodyType =>
          AbsShape(varId, varType, bodyType) ^ cv(abs)
        }
      }
      case recordLit@RecordLiteral(fields, position) =>
        val fieldsShapes = fields.flatMap((fld, p) => computeTypes(p).map((mkRecordField(fld), _))).toMap
        Some(RecordShape(None, fieldsShapes) ^ fieldsShapes.flatMap(_._2.captureSet).toSet)
      case UnitLiteral(position) => Some(UnitShape ^ Set.empty)
      case App(callee, arg, position) => {
        (computeTypes(callee), computeTypes(arg)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Type(AbsShape(varId, varType, resType), capturedByAbs)), Some(argType)) =>
            mustBeAssignable(varType, argType, arg.position, {
              Some(substitute(resType)(using Map(CapVar(varId) -> mkCapabilityPath(arg))))
            })
          case (Some(callerType), _) =>
            reportError(s"$callerType is not callable", position)
        }
      }
      case Unbox(captureSet, boxed, position) => {
        checkCaptureSet(captureSet)
        val unboxCapSet = mkCaptureSet(captureSet)
        computeTypes(boxed).flatMap {
          case Type(BoxShape(boxed), _) if boxed.captureSet == unboxCapSet => Some(boxed)
          case Type(BoxShape(boxed), _) =>
            reportError(s"illegal unboxing: the capture set ${capSetToString(boxed.captureSet)} of the unboxed type " +
              s"differs from the capture set ${capSetToString(unboxCapSet)} mentioned by the unbox term", position)
          case tpe => reportError(s"cannot unbox non-box type $tpe", position)
        }
      }
      case Let(varId, value, body, position) => {
        val valueType = computeTypes(value)
        reporter.info(s"assign ${varId.id} : ${typeDescr(valueType)}", varId.position)
        val bodyType = computeTypes(body)(using ctx.withNewBinding(varId.id, valueType))
        bodyType.foreach { bodyType =>
          if (varId.id.isFreeIn(bodyType)) {
            reportError(
              s"forbidden capture: let body has type $bodyType, which depends on let-bound variable ${varId.id.fullDescr}",
              position
            )
          }
        }
        bodyType
      }
      case Region(position) => Some(RegionShape ^ Set(RootCapability))
      case Deref(ref, position) =>
        computeTypes(ref).flatMap {
          case Type(RefShape(referenced), captureSet) => Some(referenced ^ Set.empty)
          case tpe => reportError(s"illegal dereference: $tpe is not a reference", position)
        }
      case Assign(ref, newVal, position) => {
        (computeTypes(ref), computeTypes(newVal)) match {
          case (Some(Type(RefShape(referenced), _)), Some(valType)) => {
            if valType.captureSet.isEmpty
            then mustBeAssignable(referenced, valType.shape, position, Some(UnitShape ^ Set.empty))
            else reportError(
              s"illegal assignment: capture set ${capSetToString(valType.captureSet)} of assigned value is not empty, please fix this by boxing it",
              position
            )
          }
          case (Some(nonRefType), Some(_)) =>
            reportError(s"expected a reference type as assignment target, found $nonRefType", position)
          case _ => ()
        }
        Some(UnitShape ^ Set.empty)
      }
      case Ref(regionCap, initVal, position) =>
        (computeTypes(regionCap), computeTypes(initVal)) match {
          case (Some(Type(RegionShape, _)), Some(Type(initValShape, initValCaptureSet))) =>
            if initValCaptureSet.isEmpty
            then Some(RefShape(initValShape) ^ Set(mkCapabilityPath(regionCap)))
            else reportError("reference value must have an empty capture set", position)
          case (Some(nonRegionType), _) =>
            reportError(s"expected a region, found $nonRegionType", position)
          case _ => None
        }
      case Module(regionCap, fields, position) => {
        val selfRefVar = varCreator.nextVar(SelfKw.str)
        val regionCapType = computeTypes(regionCap)
        regionCapType.foreach { regionCapType =>
          mustBeAssignable(RegionShape ^ Set(RootCapability), regionCapType, regionCap.position, None)
        }
        val substFieldsTypesOpt = fields.map(
          (fld, t) =>
            val regionCapPath = mkCapabilityPath(regionCap)
            val regPath = CapPath(CapVar(selfRefVar), RegionField)
            mkRecordField(fld) ->
              computeTypes(t).map(substitute(_)(using Map(regionCapPath -> regPath)))
        )
        val allTypesComputed = substFieldsTypesOpt.forall(_._2.isDefined)
        if allTypesComputed then
          Some(Type(
            RecordShape(Some(selfRefVar),
              substFieldsTypesOpt.map((fld, optT) => (fld, optT.get)).toMap + (RegionField -> Type(RegionShape, Set(RootCapability)))
            ), Set(RootCapability)
          ))
        else None
      }
    }
    reporter.info(s"found ${t.description} : ${typeDescr(tpe)}", t.position)
    tpe
  })

  private def cv(term: Term): Set[Capturable] = term match {
    case p: Path => Set(mkCapabilityPath(p))
    case Cap(position) => Set(RootCapability)
    case Box(boxed, position) => Set.empty
    case Abs(varId, tpe, body, position) => cv(body).filterNot(_.isRootedIn(varId.id))
    case RecordLiteral(fields, position) => fields.flatMap((_, p) => cv(p)).toSet
    case UnitLiteral(position) => Set.empty
    case App(callee, arg, position) => cv(callee) ++ cv(arg)
    case Unbox(captureSet, boxed, position) => mkCaptureSet(captureSet) ++ cv(boxed)
    case Let(varId, value, body, position) => {
      val capturedByBody = cv(body)
      if capturedByBody.exists(_.isRootedIn(varId.id))
      then cv(value) ++ cv(body).filterNot(_.isRootedIn(varId.id))
      else capturedByBody
    }
    case Region(position) => Set.empty
    case Deref(ref, position) => cv(ref)
    case Assign(ref, newVal, position) => cv(ref) ++ cv(newVal)
    case Ref(regionCap, initVal, position) => cv(regionCap) ++ cv(initVal)
    case Module(regionCap, fields, position) => cv(regionCap) ++ fields.flatMap((_, q) => cv(q))
  }

  private def mustBeAssignable(expectedType: Type, actualType: Type, pos: Position, ifAssignable: => Option[Type])
                              (using ctx: Ctx): Option[Type] = {
    val isSub = actualType.subtypeOf(expectedType)
    if (isSub) {
      ifAssignable
    } else {
      ctx.reportError(s"type mismatch: expected $expectedType, but was $actualType", pos)
    }
  }

  private def mustBeAssignable(expectedShape: ShapeType, actualShape: ShapeType, pos: Position, ifAssignable: => Option[Type])
                              (using ctx: Ctx): Option[Type] = {
    val isSub = actualShape.subshapeOf(expectedShape)
    if (isSub) {
      ifAssignable
    } else {
      ctx.reportError(s"type mismatch: expected $expectedShape, but was $actualShape", pos)
    }
  }

  private def checkCaptureSet(captureSetTree: CaptureSetTree)(using ctx: Ctx): Unit = captureSetTree match {
    case NonRootCaptureSet(capturedVarsInOrder, position) =>
      for (capVar <- capturedVarsInOrder) {
        val tpeOpt = computeTypes(capVar)
        tpeOpt.filter(_.captureSet.isEmpty).foreach { tpe =>
          ctx.reporter.warning(s"path ${pp(capVar)} of type $tpe has an empty capture set and is thus not a capability",
            capVar.position)
        }
      }
    case RootCaptureSet(position) => ()
  }

  private def typeDescr(optType: Option[Type]): String =
    optType.map(_.toString).getOrElse("??")

  private def capSetToString(capSet: Set[Capturable]): String =
    capSet.toSeq.sortBy(_.toString).mkString("{", ",", "}")

}
