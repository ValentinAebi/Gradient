package gradcc.asts

import gradcc.lang.*

trait UidTermsProvider extends TermsProvider {

  override type VarId = UniqueVarId

  override def str(varId: UniqueVarId): String = varId.toString

  def mkType(typeTree: TypeTree): Type = Type(
    mkShape(typeTree.shape),
    typeTree.captureSet.map(mkCaptureSet).getOrElse(Set.empty)
  )

  def mkShape(typeShapeTree: ShapeTree): Shape = typeShapeTree match {
    case TopShapeTree(position) => TopShape
    case AbsShapeTree(varId, varType, bodyType, position) =>
      AbsShape(varId.id, mkType(varType), mkType(bodyType))
    case BoxShapeTree(boxedType, position) => BoxShape(mkType(boxedType))
    case UnitShapeTree(position) => UnitShape
    case RefShapeTree(referencedType, position) => RefShape(mkShape(referencedType))
    case RegShapeTree(position) => RegionShape
    case RecordShapeTree(selfRef, fieldsInOrder, position) => RecordShape(
      selfRef.map(_.id),
      fieldsInOrder.map((fld, typeTree) => (mkField(fld), mkType(typeTree))).toMap
    )
  }

  def mkCaptureSet(captureSetTree: CaptureSetTree): Set[Capturable] = captureSetTree match {
    case NonRootCaptureSetTree(capturedVarsInOrder, position) =>
      capturedVarsInOrder.map(capV => mkPath(getTerm(capV))).toSet
    case RootCaptureSetTree(position) => Set(RootCapability)
  }

  def mkPath(capPath: PathTree): Path = capPath match {
    case IdentifierTree(id, position) => VarPath(id)
    case SelectTree(root, fld, position) => SelectPath(mkPath(getTerm(root)), mkField(fld))
  }

  def mkField(fld: FieldTree): RecordField = fld match {
    case NamedFieldTree(fieldName, position) => NamedField(fieldName)
    case RegFieldTree(position) => RegionField
  }
  
}
