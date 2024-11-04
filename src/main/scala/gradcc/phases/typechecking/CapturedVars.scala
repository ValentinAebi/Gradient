package gradcc.phases.typechecking

import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*

def cv(term: TermTree): CaptureDescriptor = term match {
  case p: ProperPathTree => CaptureSet(mkProperPath(p))
  case BrandedPathTree(properPath, position) => Brand
  case CapTree(position) => CaptureSet(RootCapability)
  case BoxTree(boxed, position) => CaptureSet.empty
  case AbsTree(varId, tpe, body, position) =>
    cv(body).withoutPathsRootedIn(varId.id)
  case RecordLiteralTree(fields, position) =>
    cvsFrom(fields, (fld, tpe) => cv(tpe))
  case UnitLiteralTree(position) => CaptureSet.empty
  case AppTree(callee, arg, position) => cv(callee) ++ cv(arg)
  case UnboxTree(captureDescr, boxed, position) => mkCaptureDescr(captureDescr) ++ cv(boxed)
  case let@LetTree(varId, value, typeAnnot, body, position) => {
    val capturedByBody = cv(body)
    if capturedByBody.containsPathWithRoot(varId.id)
    then cv(value) ++ cvsFrom(typeAnnot, cv) ++ cv(body).withoutPathsRootedIn(varId.id)
    else capturedByBody
  }
  case RegionTree(position) => CaptureSet.empty
  case DerefTree(ref, position) => cv(ref)
  case AssignTree(ref, newVal, position) => cv(ref) ++ cv(newVal)
  case RefTree(regionCap, initVal, position) => cv(regionCap) ++ cv(initVal)
  case ModuleTree(regionCap, fields, position) =>
    cv(regionCap) ++ cvsFrom(fields, (fld, t) => cv(t))
  case EnclosureTree(permissions, tpe, body, position) =>
    mkCaptureDescr(permissions) ++ cvsFrom(tpe.captureSet, mkCaptureDescr)
  case ObscurTree(obscured, varId, body, position) =>
    Brand
}

def cv(typeTree: TypeTree): CaptureDescriptor = {
  val TypeTree(shape, capSet, position) = typeTree
  cv(shape) ++ cvsFrom(capSet, mkCaptureDescr)
}

def cv(shapeTree: ShapeTree): CaptureDescriptor = shapeTree match {
  case TopShapeTree(position) => CaptureSet.empty
  case AbsShapeTree(varId, varType, resType, position) =>
    cv(varType) ++ cv(resType).withoutPathsRootedIn(varId.id)
  case BoxShapeTree(boxedType, position) => CaptureSet.empty
  case UnitShapeTree(position) => CaptureSet.empty
  case RefShapeTree(referencedType, position) =>
    cv(referencedType)
  case RegShapeTree(position) => CaptureSet.empty
  case RecordShapeTree(selfRef, fieldsInOrder, position) =>
    val fieldsCap = cvsFrom(fieldsInOrder, (_, tpe) => cv(tpe))
    selfRef.map(selfRef => fieldsCap.withoutPathsRootedIn(selfRef.id)).getOrElse(fieldsCap)
}

private def cvsFrom[T](iterable: Iterable[T], extractor: T => CaptureDescriptor): CaptureDescriptor =
  iterable.foldLeft[CaptureDescriptor](CaptureSet.empty)(_ ++ extractor(_))

extension (l: CaptureDescriptor) {

  private def withoutPathsRootedIn(varId: VarId): CaptureDescriptor = l match {
    case CaptureSet(captured) => CaptureSet(captured.filterNot(_.isRootedIn(varId)))
    case Brand => Brand
  }

  private def containsPathWithRoot(varId: VarId): Boolean = l match {
    case CaptureSet(paths) => paths.exists(_.isRootedIn(varId))
    case _ => false
  }

}
