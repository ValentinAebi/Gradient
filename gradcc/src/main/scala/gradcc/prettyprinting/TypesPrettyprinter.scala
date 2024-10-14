package gradcc.prettyprinting

import gradcc.lang.*
import gradcc.lang.Keyword.*

// TODO replace with proper toString

def TypesPrettyprinter(tpe: Type): String = {
  val isb = IndentedStringBuilder()
  import isb.*

  def ppT(tpe: Type): Unit = {
    val Type(shape, capt) = tpe
    ppS(shape)
    add("^{ ")
    sepList(capt.toSeq.sortBy(_.toString), ", ")(ppC)
    add(" }")
  }

  def ppS(shapeType: ShapeType): Unit = shapeType match {
    case TopShape =>
      add(TopKw)
    case AbsShape(varId, varType, resType) =>
      add(FnKw).add(" (")
      add(varId.varName)
      add(": ")
      ppT(varType)
      add(") ")
      ppT(resType)
    case BoxShape(boxed) =>
      add(BoxKw).add(" ")
      ppT(boxed)
    case UnitShape =>
      add(UnitKw)
    case RefShape(referenced) =>
      add(RefUKw).add(" ")
      ppS(referenced)
    case RegionShape =>
      add(RegUKw)
    case RecordShape(selfRef, fields) =>
      selfRef.foreach { selfRef =>
        add(SelfKw).add(" ").add(selfRef.varName).add(" ").add(InKw).add(" ")
      }
      add("{ ")
      sepList(fields.toSeq.sortBy(_.toString), ", ") { (fld, tpe) =>
        ppF(fld)
        add(": ")
        ppT(tpe)
      }
      add(" }")
  }

  def ppC(cap: Capturable): Unit = cap match {
    case CapVar(root) =>
      add(root.varName)
    case CapPath(lhs, select) =>
      ppC(lhs)
      add(".").add(select)
    case RegPath(lhs) =>
      ppC(lhs)
      add(".").add(RegLKw)
    case RootCapability =>
      add(CapKw)
  }

  def ppF(field: RecordField): Unit = field match {
    case RegularField(id) =>
      add(id)
    case RegionField =>
      add(RegUKw)
  }

  def sepList[T](ls: Seq[T], sep: String)(f: T => Unit): Unit = {
    val iter = ls.iterator
    while (iter.hasNext) {
      f(iter.next())
      if (iter.hasNext) {
        add(", ")
      }
    }
  }

  ppT(tpe)
  isb.toString
}
