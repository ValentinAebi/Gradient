package gradcc.lang

import gradcc.asts.UniqueVarId

case class Type(shape: Shape, captureSet: Set[Capturable]) {
  override def toString: String = {
    shape match {
      case absShape: AbsShape => absShape.toStringWith(captureSet)
      case _ =>
        val shapeStrRaw = shape.toString
        val shapeStr = if shapeStrRaw.contains(" ") then "(" + shapeStrRaw + ")" else shapeStrRaw
        val capSetStr = if captureSet.isEmpty then "" else s"^{${captureSet.toSeq.sortBy(_.toString).mkString(",")}}"
        shapeStr + capSetStr
    }
  }
}

sealed trait Capturable {
  def isRootedIn(varId: UniqueVarId): Boolean
}

sealed trait Path extends Capturable

case class VarPath(root: UniqueVarId) extends Path {
  override def isRootedIn(varId: UniqueVarId): Boolean = (varId == root)

  override def toString: String = root.toString
}

case class SelectPath(lhs: Path, field: RecordField) extends Path {
  override def isRootedIn(varId: UniqueVarId): Boolean = lhs.isRootedIn(varId)

  override def toString: String = s"$lhs.$field"
}

case object RootCapability extends Capturable {
  override def isRootedIn(varId: UniqueVarId): Boolean = false

  override def toString: String = "cap"
}

sealed trait Shape {
  infix def ^(captureSet: Set[Capturable]): Type = Type(this, captureSet)
}

case object TopShape extends Shape {
  override def toString: String = "Top"
}

case class AbsShape(varId: UniqueVarId, varType: Type, resType: Type) extends Shape {

  def toStringWith(capSet: Set[Capturable]): String =
    s"($varId: $varType) ->{${capSet.mkString(",")}} $resType"

  override def toString: String = toStringWith(Set.empty)
}

case class BoxShape(boxed: Type) extends Shape {
  override def toString: String = s"Box $boxed"
}

case object UnitShape extends Shape {
  override def toString: String = "Unit"
}

case class RefShape(referenced: Shape) extends Shape {
  override def toString: String = s"Ref $referenced"
}

case object RegionShape extends Shape {
  override def toString: String = "Reg"
}

case class RecordShape(selfRef: Option[UniqueVarId], fields: Map[RecordField, Type]) extends Shape {
  override def toString: String =
    selfRef.map(s => s"self $s in ").getOrElse("") ++
      s"{${fields.toSeq.map((fld, tpe) => s"$fld : $tpe").mkString(", ")}}"
}
