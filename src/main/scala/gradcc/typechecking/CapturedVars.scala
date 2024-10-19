package gradcc.typechecking

import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.{Capturable, RootCapability}

def cv(term: Term): Set[Capturable] = term match {
  case p: Path => Set(mkCapabilityPath(p))
  case Cap(position) => Set(RootCapability)
  case Box(boxed, position) => Set.empty
  case Abs(varId, tpe, body, position) => cv(body).filterNot(_.isRootedIn(varId.id))
  case RecordLiteral(fields, position) => fields.flatMap((_, p) => cv(p)).toSet
  case UnitLiteral(position) => Set.empty
  case App(callee, arg, position) => cv(callee) ++ cv(arg)
  case Unbox(captureSet, boxed, position) => mkCaptureSet(captureSet) ++ cv(boxed)
  case let@Let(varId, value, typeAnnot, body, position) => {
    val capturedByBody = cv(body)
    if capturedByBody.exists(_.isRootedIn(varId.id))
    then cv(value) ++ typeAnnot.toSeq.flatMap(cv) ++ cv(body).filterNot(_.isRootedIn(varId.id))
    else capturedByBody
  }
  case Region(position) => Set.empty
  case Deref(ref, position) => cv(ref)
  case Assign(ref, newVal, position) => cv(ref) ++ cv(newVal)
  case Ref(regionCap, initVal, position) => cv(regionCap) ++ cv(initVal)
  case Module(regionCap, fields, position) => cv(regionCap) ++ fields.flatMap((_, q) => cv(q))
}

def cv(typeTree: TypeTree): Set[Capturable] = {
  val TypeTree(shape, capSet, position) = typeTree
  cv(shape) ++ capSet.toSeq.flatMap(cv)
}

def cv(shapeTree: TypeShapeTree): Set[Capturable] = shapeTree match {
  case TopTypeTree(position) => Set.empty
  case AbsTypeTree(varId, varType, resType, position) =>
    cv(varType) ++ cv(resType).filterNot(_.isRootedIn(varId.id))
  case BoxTypeTree(boxedType, position) => Set.empty
  case UnitTypeTree(position) => Set.empty
  case RefTypeTree(referencedType, position) =>
    cv(referencedType)
  case RegTypeTree(position) => Set.empty
  case RecordTypeTree(selfRef, fieldsInOrder, position) =>
    val fieldsCap = fieldsInOrder.flatMap((_, tpe) => cv(tpe)).toSet
    selfRef.map(selfRef => fieldsCap.filterNot(_.isRootedIn(selfRef.id))).getOrElse(fieldsCap)
}

def cv(captureSetTree: CaptureSetTree): Set[Capturable] = captureSetTree match {
  case NonRootCaptureSet(capturedVarsInOrder, position) =>
    capturedVarsInOrder.map(capV => mkCapabilityPath(capV)).toSet
  case RootCaptureSet(position) =>
    Set.empty
}
