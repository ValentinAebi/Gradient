package gradcc.typechecking

import commons.{Position, Reporter, SimplePhase}
import gradcc.asts
import gradcc.asts.*
import gradcc.lang.*

import scala.collection.mutable


final class TypeCheckerPhase extends SimplePhase[Term, Map[Term, Type]]("Typechecker") {

  private type TermsTypes = mutable.Map[Term, Option[Type]]
  private type Store = Map[String, Type]

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

    val inferredType = ctx.types.getOrElseUpdate(t, {
      t match {
        case Identifier(id, position) => store.get(id).orElse(reportError(s"not found: $id", position))
        case Cap(position) => ???
        case Select(owner, fieldId, position) =>
          computeTypes(owner).flatMap {
            case Type(RecordType(fields, selfRef), captureSet) if fields.contains(fieldId) =>
              fields.get(fieldId)
            case otherType => reportError(
              s"no field named '$fieldId' found in owner type $otherType", position)
          }
        case Box(boxed, position) => ???
        case abs@Abs(varIdent, varTypeTree, body, position) => {
          val varId = varIdent.id
          val varType = mkType(varTypeTree)
          computeTypes(body)(using ctx.withNewBinding(varId, varType)).map { bodyType =>
            AbsType(varId, varType, bodyType) ^ cv(abs)
          }
        }
        case RecordLiteral(fields, position) => ???
        case UnitLiteral(position) => Some(UnitType ^ Set.empty)
        case App(callee, arg, position) => ???
        case Unbox(captureSet, boxed, position) => ???
        case Let(varId, value, body, position) => ???
        case Region(position) => Some(RegionType ^ Set(RootCapability))
        case Deref(ref, position) => ???
        case Assign(ref, newVal, position) => ???
        case Ref(regionCap, initVal, position) => ???
        case Modif(regionCap, fields, position) => ???
      }
    })
    ???
  })

  private case class Ctx(store: Store, types: TermsTypes, reporter: Reporter) {

    def withNewBinding(varName: String, varType: Type): Ctx = copy(store = store + (varName -> varType))

    def reportError(msg: String, pos: Position): None.type = {
      reporter.error(msg, pos)
      None
    }
  }

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
    case Modif(regionCap, fields, position) => cv(regionCap) ++ fields.flatMap((_, q) => cv(q))
  }

  private def mkType(typeTree: TypeTree): Type = Type(
    mkShape(typeTree.shape),
    typeTree.captureSet.map(mkCaptureSet).getOrElse(Set.empty)
  )

  private def mkShape(typeShapeTree: TypeShapeTree): ShapeType = typeShapeTree match {
    case TopTypeTree(position) => TopType
    case AbsTypeTree(varId, varType, bodyType, position) =>
      AbsType(varId.id, mkType(varType), mkType(bodyType))
    case BoxTypeTree(boxedType, position) => BoxType(mkType(boxedType))
    case UnitTypeTree(position) => UnitType
    case RefTypeTree(referencedType, position) => RefType(mkShape(referencedType))
    case RegTypeTree(position) => RegionType
    case RecordTypeTree(fieldsInOrder, selfRef, position) => RecordType(
      fieldsInOrder.map((id, typeTree) => (id.id, mkType(typeTree))).toMap,
      selfRef.map(_.id)
    )
  }

  private def mkCaptureSet(captureSetTree: CaptureSetTree): Set[Capturable] = captureSetTree match {
    case ExplicitCaptureSetTree(capturedVarsInOrder, position) => capturedVarsInOrder.map(mkCapabilityPath).toSet
    case ImplicitCaptureSetTree(position) => Set(RootCapability)
  }

  private def mkCapabilityPath(capPath: Path): CapabilityPath = {
    val root :: selects = flattenSelectsReversed(capPath).reverse
    CapabilityPath(root, selects)
  }

  private def flattenSelectsReversed(p: Path): List[String] = p match {
    case Identifier(id, position) => List(id)
    case Select(root, select, position) => select :: flattenSelectsReversed(root)
  }

}
