package gradcc.typechecking

import gradcc.*
import gradcc.asts.{TypedTerm, TypedTerms as T, UniquelyNamedTerms as U}
import gradcc.lang.*
import gradcc.prettyprinting.TermsPrettyprinter
import gradcc.typechecking.SubtypingRelation.*

// TODO pack and unpack
// TODO double-check every typing rule

final class TypeCheckerPhase extends SimplePhase[U.Term, TypedTerm[T.Term]]("Typechecker") {
  private val varCreator = SyntheticVarCreator()

  private val pp: TypedTerm[T.Term] => String = TermsPrettyprinter(T)

  override val acceptsFaultyInput: Boolean = false

  override protected def runImpl(in: U.Term, reporter: Reporter): TypedTerm[T.Term] =
    typeTerm(in)(using Ctx(Map.empty, reporter))

  private def typeTerm(t: U.Term)(using ctx: Ctx): TypedTerm[T.Term] = t match {
    case p: U.Path => typePath(p)
    case U.Cap(position) =>
      throw AssertionError("unexpected type computation on the root capability")
    case abs@U.Abs(varIdent, varTypeTree, body, position) => {
      val varType = U.mkType(varTypeTree)
      val varId = varIdent.id
      val typedBody = typeTerm(body)(using ctx.withNewBinding(varId, Some(varType)))
      T.Abs(convertIdent(varIdent), typeTypeTree(varTypeTree), typedBody, position).withType(
        typedBody.tpe.map(AbsShape(varId, varType, _) ^ cv(abs))
      )
    }
    case U.Box(boxed, position) =>
      val typedBoxed = typePath(boxed)
      T.Box(typedBoxed, position).withType(
        typedBoxed.tpe.map(BoxShape(_) ^ Set.empty)
      )
    case recordLit@U.RecordLiteral(fields, position) =>
      val typedFields = convertFields(fields, typePath)
      T.RecordLiteral(typedFields, position).withType {
        collectTypes(typedFields).map { fldTypes =>
          val fieldsToType = fields.map((fld, _) => U.mkField(fld)).zip(fldTypes).toMap
          val recordCapSet = fldTypes.flatMap(_.captureSet).toSet
          RecordShape(None, fieldsToType) ^ recordCapSet
        }
      }
    case U.UnitLiteral(position) =>
      T.UnitLiteral(position).withType(UnitShape ^ Set.empty)
    case U.App(callee, arg, position) => {
      val typedCallee = typePath(callee)
      val typedArg = typePath(arg)
      val tpeOpt = (typedCallee.tpe, typedArg.tpe) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(Type(AbsShape(varId, varType, resType), capturedByAbs)), Some(argType)) =>
          mustBeAssignable(varType, argType, arg.position, {
            Some(substitute(resType)(using Map(CapVar(varId) -> U.mkCapabilityPath(arg))))
          })
        case (Some(callerType), _) =>
          ctx.reportError(s"$callerType is not callable", position)
      }
      T.App(typedCallee, typedArg, position).withType(tpeOpt)
    }
    case U.Unbox(captureSet, boxed, position) => {
      val typedCapSet = typeCaptureSet(captureSet)
      val typedBoxed = typePath(boxed)
      val unboxCapSet = T.mkCaptureSet(typedCapSet)
      T.Unbox(typedCapSet, typedBoxed, position).withType(
        typedBoxed.tpe.flatMap {
          case Type(BoxShape(boxed), _) if boxed.captureSet == unboxCapSet => Some(boxed)
          case Type(BoxShape(boxed), _) =>
            ctx.reportError(s"illegal unboxing: the capture set ${capSetToString(boxed.captureSet)} of the unboxed type " +
              s"differs from the capture set ${capSetToString(unboxCapSet)} mentioned by the unbox term", position)
          case tpe => ctx.reportError(s"cannot unbox non-box type $tpe", position)
        }
      )
    }
    case U.Let(varId, value, typeAnnot, body, position) => {
      val typedValue = typeTerm(value)
      for (rawValueType <- typedValue.tpe; typeAnnot <- typeAnnot) {
        mustBeAssignable(U.mkType(typeAnnot), rawValueType, typeAnnot.position, None)
      }
      if (typeAnnot.isEmpty) {
        ctx.reporter.info(s"assign ${varId.id} : ${typeDescr(typedValue.tpe)}", varId.position)
      }
      val valueType = typeAnnot.map(U.mkType).orElse(typedValue.tpe)
      val typedBody = typeTerm(body)(using ctx.withNewBinding(varId.id, valueType))
      typedBody.tpe.foreach { bodyType =>
        if (varId.id.isFreeIn(bodyType)) {
          ctx.reportError(
            s"forbidden capture: let body has type $bodyType, which depends on let-bound variable ${varId.id.fullDescr}",
            position
          )
        }
      }
      T.Let(convertIdent(varId), typedValue, typeAnnot.map(typeTypeTree), typedBody, position).withType(typedBody.tpe)
    }
    case U.Region(position) =>
      T.Region(position).withType(RegionShape ^ Set(RootCapability))
    case U.Deref(ref, position) =>
      val typedRef = typePath(ref)
      T.Deref(typedRef, position).withType(
        typedRef.tpe.flatMap {
          case Type(RefShape(referenced), captureSet) => Some(referenced ^ Set.empty)
          case tpe => ctx.reportError(s"illegal dereference: $tpe is not a reference", position)
        }
      )
    case U.Assign(ref, newVal, position) => {
      val typedRef = typePath(ref)
      val typedNewVal = typePath(newVal)
      (typedRef.tpe, typedNewVal.tpe) match {
        case (Some(Type(RefShape(referenced), _)), Some(valType)) => {
          if valType.captureSet.isEmpty
          then mustBeAssignable(referenced, valType.shape, position, Some(UnitShape ^ Set.empty))
          else ctx.reportError(
            s"illegal assignment: capture set ${capSetToString(valType.captureSet)} of assigned value is not empty, please fix this by boxing it",
            position
          )
        }
        case (Some(nonRefType), Some(_)) =>
          ctx.reportError(s"expected a reference type as assignment target, found $nonRefType", position)
        case _ => ()
      }
      T.Assign(typedRef, typedNewVal, position).withType(UnitShape ^ Set.empty)
    }
    case U.Ref(regionCap, initVal, position) =>
      val typedRegionCap = typePath(regionCap)
      val typedInitVal = typePath(initVal)
      val tpeOpt = (typedRegionCap.tpe, typedInitVal.tpe) match {
        case (Some(Type(RegionShape, _)), Some(Type(initValShape, initValCaptureSet))) =>
          if initValCaptureSet.isEmpty
          then Some(RefShape(initValShape) ^ Set(U.mkCapabilityPath(regionCap)))
          else ctx.reportError("reference value must have an empty capture set", position)
        case (Some(nonRegionType), _) =>
          ctx.reportError(s"expected a region, found $nonRegionType", position)
        case _ => None
      }
      T.Ref(typedRegionCap, typedInitVal, position).withType(tpeOpt)
    case U.Module(regionCap, fields, position) => {
      val typedRegionCap = typePath(regionCap)
      typedRegionCap.tpe.foreach { regionCapType =>
        mustBeAssignable(RegionShape ^ Set(RootCapability), regionCapType, regionCap.position, None)
      }
      val selfRefVar = varCreator.nextVar(Keyword.SelfKw.str)
      val regionCapPath = U.mkCapabilityPath(regionCap)
      val substFields = fields.map(
        (fld, p) =>
          val regPath = CapPath(CapVar(selfRefVar), RegionField)
          val TypedTerm(convP, pType) = typePath(p)
          convertField(fld) -> TypedTerm(convP, pType.map(substitute(_)(using Map(regionCapPath -> regPath))))
      )
      val tpeOpt = collectTypes(substFields).map { fieldTypes =>
        val fieldsToTypes = substFields.map((fld, _) => T.mkField(fld)).zip(fieldTypes).toMap
        RecordShape(Some(selfRefVar), fieldsToTypes) ^ Set(RootCapability)
      }
      T.Module(typedRegionCap, substFields, position).withType(tpeOpt)
    }
  }

  private def typePath(p: U.Path)(using ctx: Ctx): TypedTerm[T.Path] = p match {
    case U.Identifier(id, position) =>
      // id must be found, o.w. the renaming phase stops the pipeline before it reaches this point
      T.Identifier(id, position).withType(ctx.varLookup(id))
    case U.Select(lhs, field, position) =>
      val typedLhs = typePath(lhs)
      val fld = U.mkField(field)
      T.Select(typedLhs, convertField(field), position).withType(
        typedLhs.tpe.flatMap {
          case Type(RecordShape(selfRef, fields), captureSet) if fields.contains(fld) =>
            Some(fields.apply(fld))
          case otherType =>
            ctx.reportError(s"no '$fld' field found in owner type $otherType", position)
        }
      )
  }

  private def convertIdent(ident: U.Identifier)(using Ctx): T.Identifier = {
    val U.Identifier(id, position) = ident
    T.Identifier(id, position)
  }

  private def typeTypeTree(tt: U.TypeTree)(using Ctx): T.TypeTree = {
    val U.TypeTree(shape, captureSet, position) = tt
    T.TypeTree(typeShapeTree(shape), captureSet.map(typeCaptureSet), position)
  }

  private def typeShapeTree(shape: U.TypeShapeTree)(using Ctx): T.TypeShapeTree = shape match {
    case U.TopTypeTree(position) =>
      T.TopTypeTree(position)
    case U.AbsTypeTree(varId, varType, resType, position) =>
      T.AbsTypeTree(convertIdent(varId), typeTypeTree(varType), typeTypeTree(resType), position)
    case U.BoxTypeTree(boxedType, position) =>
      T.BoxTypeTree(typeTypeTree(boxedType), position)
    case U.UnitTypeTree(position) =>
      T.UnitTypeTree(position)
    case U.RefTypeTree(referencedType, position) =>
      T.RefTypeTree(typeShapeTree(referencedType), position)
    case U.RegTypeTree(position) =>
      T.RegTypeTree(position)
    case U.RecordTypeTree(selfRef, fieldsInOrder, position) =>
      T.RecordTypeTree(
        selfRef.map(convertIdent),
        convertFields(fieldsInOrder, typeTypeTree),
        position
      )
  }

  private def typeCaptureSet(capSet: U.CaptureSetTree)(using ctx: Ctx): T.CaptureSetTree = capSet match {
    case U.NonRootCaptureSet(capturedVarsInOrder, position) =>
      val typedCapturedVars = capturedVarsInOrder.map(typePath)
      typedCapturedVars.filter(_.tpe.exists(_.captureSet.isEmpty)).foreach { capVar =>
        ctx.reporter.warning(s"path ${pp(capVar)} has an empty capture set and is thus not a capability",
          capVar.term.position)
      }
      T.NonRootCaptureSet(typedCapturedVars, position)
    case U.RootCaptureSet(position) =>
      T.RootCaptureSet(position)
  }

  private def convertField(fld: U.FieldTree)(using Ctx): T.FieldTree = fld match {
    case U.NamedFieldTree(fieldName, position) =>
      T.NamedFieldTree(fieldName, position)
    case U.RegFieldTree(position) =>
      T.RegFieldTree(position)
  }

  private def convertFields[A, B](fields: Seq[(U.FieldTree, A)], typingFunc: A => B)(using Ctx): Seq[(T.FieldTree, B)] =
    fields.map((fld, a) => (convertField(fld), typingFunc(a)))

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

  extension [A <: T.Term](term: A) {
    private def withType(tpe: Option[Type]): TypedTerm[A] = TypedTerm(term, tpe)
    private def withType(tpe: Type): TypedTerm[A] = term.withType(Some(tpe))
  }

  private def typeDescr(optType: Option[Type]): String =
    optType.map(_.toString).getOrElse("??")

  private def capSetToString(capSet: Set[Capturable]): String =
    capSet.toSeq.sortBy(_.toString).mkString("{", ",", "}")

  private def collectTypes[B <: T.Term](seq: Seq[(T.FieldTree, TypedTerm[B])]): Option[Seq[Type]] = {
    seq.foldRight(Option(Nil)) {
      case ((_, TypedTerm(_, tpe)), acc) => for tpe <- tpe; tail <- acc yield tpe :: tail
    }
  }

}
