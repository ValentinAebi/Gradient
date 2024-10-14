package gradcc.lang

import gradcc.asts.UniqueVarId

case class Type(shape: ShapeType, captureSet: Set[Capturable])

sealed trait Capturable {
  def isRootedIn(varId: UniqueVarId): Boolean
}

sealed trait CapabilityPath extends Capturable

case class CapVar(root: UniqueVarId) extends CapabilityPath {
  override def isRootedIn(varId: UniqueVarId): Boolean = (varId == root)
}

case class CapPath(lhs: CapabilityPath, select: String) extends CapabilityPath {
  override def isRootedIn(varId: UniqueVarId): Boolean = lhs.isRootedIn(varId)
}

case class RegPath(lhs: CapabilityPath) extends CapabilityPath {
  override def isRootedIn(varId: UniqueVarId): Boolean = lhs.isRootedIn(varId)
}

case object RootCapability extends Capturable {
  override def isRootedIn(varId: UniqueVarId): Boolean = false
}

sealed trait ShapeType {
  infix def ^(captureSet: Set[Capturable]): Type = Type(this, captureSet)
}

case object TopShape extends ShapeType
case class AbsShape(varId: UniqueVarId, varType: Type, resType: Type) extends ShapeType
case class BoxShape(boxed: Type) extends ShapeType
case object UnitShape extends ShapeType
case class RefShape(referenced: ShapeType) extends ShapeType
case object RegionShape extends ShapeType
case class RecordShape(selfRef: Option[UniqueVarId], fields: Map[RecordField, Type]) extends ShapeType

sealed trait RecordField
case class RegularField(id: String) extends RecordField
case object RegionField extends RecordField
