package gradcc.phases.typechecking

import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.{Capturable, RootCapability}

def cv(term: TermTree): Set[Capturable] = term match {
  case p: PathTree => Set(mkPath(p))
  case CapTree(position) => Set(RootCapability)
  case BoxTree(boxed, position) => Set.empty
  case AbsTree(varId, tpe, body, position) => cv(body).filterNot(_.isRootedIn(varId.id))
  case RecordLiteralTree(fields, position) => fields.flatMap((_, p) => cv(p)).toSet
  case UnitLiteralTree(position) => Set.empty
  case AppTree(callee, arg, position) => cv(callee) ++ cv(arg)
  case UnboxTree(captureSet, boxed, position) => mkCaptureSet(captureSet) ++ cv(boxed)
  case let@LetTree(varId, value, typeAnnot, body, position) => {
    val capturedByBody = cv(body)
    if capturedByBody.exists(_.isRootedIn(varId.id))
    then cv(value) ++ typeAnnot.toSeq.flatMap(cv) ++ cv(body).filterNot(_.isRootedIn(varId.id))
    else capturedByBody
  }
  case RegionTree(position) => Set.empty
  case DerefTree(ref, position) => cv(ref)
  case AssignTree(ref, newVal, position) => cv(ref) ++ cv(newVal)
  case RefTree(regionCap, initVal, position) => cv(regionCap) ++ cv(initVal)
  case ModuleTree(regionCap, fields, position) => cv(regionCap) ++ fields.flatMap((_, q) => cv(q))
}

def cv(typeTree: TypeTree): Set[Capturable] = {
  val TypeTree(shape, capSet, position) = typeTree
  cv(shape) ++ capSet.toSeq.flatMap(cv)
}

def cv(shapeTree: ShapeTree): Set[Capturable] = shapeTree match {
  case TopShapeTree(position) => Set.empty
  case AbsShapeTree(varId, varType, resType, position) =>
    cv(varType) ++ cv(resType).filterNot(_.isRootedIn(varId.id))
  case BoxShapeTree(boxedType, position) => Set.empty
  case UnitShapeTree(position) => Set.empty
  case RefShapeTree(referencedType, position) =>
    cv(referencedType)
  case RegShapeTree(position) => Set.empty
  case RecordShapeTree(selfRef, fieldsInOrder, position) =>
    val fieldsCap = fieldsInOrder.flatMap((_, tpe) => cv(tpe)).toSet
    selfRef.map(selfRef => fieldsCap.filterNot(_.isRootedIn(selfRef.id))).getOrElse(fieldsCap)
}

def cv(captureSetTree: CaptureSetTree): Set[Capturable] = captureSetTree match {
  case NonRootCaptureSetTree(capturedVarsInOrder, position) =>
    capturedVarsInOrder.map(capV => mkPath(capV)).toSet
  case RootCaptureSetTree(position) =>
    Set.empty
}
