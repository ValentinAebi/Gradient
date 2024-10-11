package gradcc.typechecking

import gradcc.{Position, Reporter, SimplePhase}
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*
import gradcc.typechecking.SubtypingRelation.subtypeOf
import gradcc.{asts, lang}

import scala.collection.mutable


final class TypeCheckerPhase extends SimplePhase[Term, Map[Term, Type]]("Typechecker") {

  override protected def runImpl(in: Term, reporter: Reporter): Map[Term, Type] = {
    val types: TermsTypes = mutable.Map.empty
    computeTypes(in)(using Ctx(Map.empty, types, reporter))
    types.flatMap {
      case (_, None) => None
      case (term, Some(tpe)) => Some(term -> tpe)
    }.toMap
  }

  private def computeTypes(t: Term)(using ctx: Ctx): Option[Type] = ctx.types.getOrElseUpdate(t, {
    import ctx.*
    t match {
      case Identifier(id, position) =>
        // id must be found, o.w. the renaming phase stops the pipeline before it reaches this point
        store.apply(id)
      case Cap(position) => ???
      case Select(owner, fieldId, position) =>
        computeTypes(owner).flatMap {
          case Type(RecordShape(selfRef, fields), captureSet) if fields.contains(RegularField(fieldId)) =>
            fields.get(RegularField(fieldId))
          case otherType => reportError(
            s"no field named '$fieldId' found in owner type $otherType", position)
        }
      case Box(boxed, position) => ???
      case abs@Abs(varIdent, varTypeTree, body, position) => {
        val varId = varIdent.id
        val varType = mkType(varTypeTree)
        computeTypes(body)(using ctx.withNewBinding(varId, Some(varType))).map { bodyType =>
          AbsShape(varId, varType, bodyType) ^ cv(abs)
        }
      }
      case recordLit@RecordLiteral(selfRef, fields, position) => Some(
        RecordShape(
          selfRef.map(_.id),
          fields.flatMap((fld, p) => computeTypes(p).map((mkRecordField(fld), _))).toMap,
        ) ^ cv(recordLit)
      )
      case UnitLiteral(position) => Some(UnitShape ^ Set.empty)
      case App(callee, arg, position) => {
        (computeTypes(callee), computeTypes(arg)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Type(AbsShape(varId, varType, resType), capturedByAbs)), Some(argType)) =>
            mustBeAssignable(varType, argType, arg.position) {
              Some(substitute(resType)(using Map(varId -> arg)))
            }
          case (Some(callerType), _) =>
            reportError(s"$callerType is not callable", position)
        }
      }
      case Unbox(captureSet, boxed, position) => ???
      case Let(varId, value, body, position) => {
        val valueType = computeTypes(value)
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
      case Deref(ref, position) => ???
      case Assign(ref, newVal, position) => ???
      case Ref(regionCap, initVal, position) => ???
      case Modif(regionCap, fields, position) => ???
    }
  })

  private def cv(term: Term): Set[Capturable] = term match {
    case p: Path => Set(mkCapabilityPath(p))
    case Cap(position) => Set(RootCapability)
    case Box(boxed, position) => Set.empty
    case Abs(varId, tpe, body, position) => cv(body).filterNot(_.isRootedIn(varId.id))
    case RecordLiteral(selfRef, fields, position) =>
      fields.flatMap((_, p) => cv(p)).toSet
        .filterNot(capturable => selfRef.exists(sr => capturable.isRootedIn(sr.id)))
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
    case Modif(regionCap, fields, position) => cv(regionCap) ++ fields.flatMap((_, q) => cv(q))
  }

  private def mustBeAssignable(expectedType: Type, actualType: Type, pos: Position)
                              (ifAssignable: => Option[Type])
                              (using ctx: Ctx): Option[Type] = {
    val isSub = actualType.subtypeOf(expectedType)
    if (isSub) {
      ifAssignable
    } else {
      ctx.reportError(s"type mismatch: expected $expectedType, but was $actualType", pos)
    }
  }

}
