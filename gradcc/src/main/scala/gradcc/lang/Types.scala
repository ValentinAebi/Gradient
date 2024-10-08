package gradcc.lang

case class Type(shape: ShapeType, captureSet: Set[Capturable])

sealed trait Capturable {
  def isRootedIn(varId: String): Boolean
}

case class CapabilityPath(root: String, selects: Seq[String]) extends Capturable {
  override def isRootedIn(varId: String): Boolean = (varId == root)
}

case object RootCapability extends Capturable {
  override def isRootedIn(varId: String): Boolean = false
}

sealed trait ShapeType {
  infix def ^(captureSet: Set[Capturable]): Type = Type(this, captureSet)
}

case object TopType extends ShapeType
case class AbsType(varId: String, varType: Type, resType: Type) extends ShapeType
case class BoxType(boxed: Type) extends ShapeType
case object UnitType extends ShapeType
case class RefType(referenced: ShapeType) extends ShapeType
case object RegionType extends ShapeType
case class RecordType(fields: Map[String, Type], selfRef: Option[String]) extends ShapeType
