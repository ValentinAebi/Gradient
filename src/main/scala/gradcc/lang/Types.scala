package gradcc.lang

import gradcc.asts.UniqueVarId

case class Type(shape: Shape, captureDescr: CaptureDescriptor) {
  
  def isPure: Boolean = captureDescr.isCapSetOfPureType
  
  override def toString: String = {
    shape match {
      case absShape: AbsShape => absShape.toStringWith(captureDescr)
      case _ =>
        val shapeStrRaw = shape.toString
        val shapeStr = if shapeStrRaw.contains(" ") then "(" + shapeStrRaw + ")" else shapeStrRaw
        shapeStr + captureDescr.toHatNotation
    }
  }
}

sealed trait CaptureDescriptor {
  def isCapSetOfPureType: Boolean
  def prettified: String
  def toHatNotation: String
  
  infix def ++(that: CaptureDescriptor): CaptureDescriptor = (this, that) match {
    case (CaptureSet(l), CaptureSet(r)) => CaptureSet(l ++ r)
    case _ => Brand
  }
  
  def removed(rem: Iterable[Capturable]): CaptureDescriptor
  
}

case class CaptureSet(captured: Set[Capturable]) extends CaptureDescriptor {
  override def isCapSetOfPureType: Boolean = captured.isEmpty
  override def removed(rem: Iterable[Capturable]): CaptureSet = CaptureSet(captured -- rem)
  override def prettified: String = {
    if captured.isEmpty then ""
    else captured.mkString("{", ",", "}")
  }
  override def toHatNotation: String = {
    if captured.isEmpty then ""
    else captured.mkString("^{", ",", "}")
  }
  override def toString: String = captured.mkString("{", ",", "}")
}

object CaptureSet {
  def apply(capt: Capturable*): CaptureSet = CaptureSet(capt.toSet)
  def empty: CaptureSet = CaptureSet(Set.empty)
}

case object Brand extends CaptureDescriptor {
  override def isCapSetOfPureType: Boolean = false
  override def removed(rem: Iterable[Capturable]): Brand.type = this
  override def prettified: String = "#"
  override def toHatNotation: String = "^#"
  override def toString: String = "#"
}

sealed trait Capturable {
  def isRootedIn(varId: UniqueVarId): Boolean
}

sealed trait StablePath

sealed trait ProperPath extends StablePath, Capturable

case class VarPath(root: UniqueVarId) extends ProperPath {
  override def isRootedIn(varId: UniqueVarId): Boolean = (varId == root)
  override def toString: String = root.toString
}

case class SelectPath(lhs: ProperPath, field: RecordField) extends ProperPath {
  override def isRootedIn(varId: UniqueVarId): Boolean = lhs.isRootedIn(varId)
  override def toString: String = s"$lhs.$field"
}

case class BrandedPath(p: ProperPath) extends StablePath {
  export p.isRootedIn
  override def toString: String = s"#$p"
}

case object RootCapability extends Capturable {
  override def isRootedIn(varId: UniqueVarId): Boolean = false
  override def toString: String = "cap"
}

sealed trait Shape {
  infix def ^(captureDescriptor: CaptureDescriptor): Type = Type(this, captureDescriptor)
}

case object TopShape extends Shape {
  override def toString: String = "Top"
}

case class AbsShape(varId: UniqueVarId, varType: Type, resType: Type) extends Shape {

  def toStringWith(capDescr: CaptureDescriptor): String =
    s"($varId: $varType) ->${capDescr.prettified} $resType"

  override def toString: String = s"($varId: $varType) -> $resType"
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
