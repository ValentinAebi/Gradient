package gradcc.typechecking

import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*

def mkRecordField(fld: Field): RecordField = fld match {
  case NamedField(fieldName, position) => RegularField(fieldName)
  case Reg(position) => RegionField
}

def mkType(typeTree: TypeTree): Type = Type(
  mkShape(typeTree.shape),
  typeTree.captureSet.map(mkCaptureSet).getOrElse(Set.empty)
)

def mkShape(typeShapeTree: TypeShapeTree): ShapeType = typeShapeTree match {
  case TopTypeTree(position) => TopShape
  case AbsTypeTree(varId, varType, bodyType, position) =>
    AbsShape(varId.id, mkType(varType), mkType(bodyType))
  case BoxTypeTree(boxedType, position) => BoxShape(mkType(boxedType))
  case UnitTypeTree(position) => UnitShape
  case RefTypeTree(referencedType, position) => RefShape(mkShape(referencedType))
  case RegTypeTree(position) => RegionShape
  case RecordTypeTree(selfRef, fieldsInOrder, position) => RecordShape(
    selfRef.map(_.id),
    fieldsInOrder.map((fld, typeTree) => (mkRecordField(fld), mkType(typeTree))).toMap
  )
}

def mkCaptureSet(captureSetTree: CaptureSetTree): Set[Capturable] = captureSetTree match {
  case ExplicitCaptureSetTree(capturedVarsInOrder, position) => capturedVarsInOrder.map(mkCapabilityPath).toSet
  case ImplicitCaptureSetTree(position) => Set(RootCapability)
}

def mkCapabilityPath(capPath: Path): CapabilityPath = capPath match {
  case Identifier(id, position) => CapVar(id)
  case Select(root, select, position) => CapPath(mkCapabilityPath(root), select)
}
