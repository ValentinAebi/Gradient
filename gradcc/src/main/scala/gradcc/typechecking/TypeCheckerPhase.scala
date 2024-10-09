package gradcc.typechecking

import commons.{Reporter, SimplePhase}
import gradcc.asts.UniqueVarId
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
      case Identifier(id, position) => store.get(id).orElse(reportError(s"not found: $id", position))
      case Cap(position) => ???
      case Select(owner, fieldId, position) =>
        computeTypes(owner).flatMap {
          case Type(RecordShape(fields, selfRef), captureSet) if fields.contains(RegularField(fieldId)) =>
            fields.get(RegularField(fieldId))
          case otherType => reportError(
            s"no field named '$fieldId' found in owner type $otherType", position)
        }
      case Box(boxed, position) => ???
      case abs@Abs(varIdent, varTypeTree, body, position) => {
        val varId = varIdent.id
        val varType = mkType(varTypeTree)
        computeTypes(body)(using ctx.withNewBinding(varId, varType)).map { bodyType =>
          AbsShape(varId, varType, bodyType) ^ cv(abs)
        }
      }
      case recordLit@RecordLiteral(fields, selfRef, position) => Some(
        RecordShape(
          fields.flatMap((fld, p) => computeTypes(p).map((mkRecordField(fld), _))).toMap,
          selfRef.map(_.id)
        ) ^ cv(recordLit)
      )
      case UnitLiteral(position) => Some(UnitShape ^ Set.empty)
      case App(callee, arg, position) => {
        (computeTypes(callee), computeTypes(arg)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Type(AbsShape(varId, varType, resType), capturedByAbs)), Some(argType)) => {
            val argMatchesParam = argType.subtypeOf(varType)
            ???
          }
          case (Some(callerType), Some(argType)) => ???

        }
      }
      case Unbox(captureSet, boxed, position) => ???
      case Let(varId, value, body, position) => ???
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
    case RecordLiteral(fields, selfRef, position) =>
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

  private def mkRecordField(fld: Field): RecordField = fld match {
    case NamedField(fieldName, position) => RegularField(fieldName)
    case Reg(position) => RegionField
  }

  private def mkType(typeTree: TypeTree): Type = Type(
    mkShape(typeTree.shape),
    typeTree.captureSet.map(mkCaptureSet).getOrElse(Set.empty)
  )

  private def mkShape(typeShapeTree: TypeShapeTree): ShapeType = typeShapeTree match {
    case TopTypeTree(position) => TopShape
    case AbsTypeTree(varId, varType, bodyType, position) =>
      AbsShape(varId.id, mkType(varType), mkType(bodyType))
    case BoxTypeTree(boxedType, position) => BoxShape(mkType(boxedType))
    case UnitTypeTree(position) => UnitShape
    case RefTypeTree(referencedType, position) => RefShape(mkShape(referencedType))
    case RegTypeTree(position) => RegionShape
    case RecordTypeTree(fieldsInOrder, selfRef, position) => RecordShape(
      fieldsInOrder.map((fld, typeTree) => (mkRecordField(fld), mkType(typeTree))).toMap,
      selfRef.map(_.id)
    )
  }

  private def mkCaptureSet(captureSetTree: CaptureSetTree): Set[Capturable] = captureSetTree match {
    case ExplicitCaptureSetTree(capturedVarsInOrder, position) => capturedVarsInOrder.map(mkCapabilityPath).toSet
    case ImplicitCaptureSetTree(position) => Set(RootCapability)
  }

  private def mkCapabilityPath(capPath: Path): CapabilityPath = {
    val (root, selectsReversed) = flattenSelectsReversed(capPath)
    CapabilityPath(root, selectsReversed.reverse)
  }

  private def flattenSelectsReversed(p: Path): (UniqueVarId, List[String]) = p match {
    case Identifier(id, position) => (id, Nil)
    case Select(root, select, position) => {
      val (id, previousSelects) = flattenSelectsReversed(root)
      (id, select :: previousSelects)
    }
  }

}
