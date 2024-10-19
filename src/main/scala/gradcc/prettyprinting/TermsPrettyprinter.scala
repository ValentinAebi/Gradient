package gradcc.prettyprinting

import gradcc.asts.TermsProvider
import gradcc.lang.Keyword.*
import gradcc.lang.ShapeType


def TermsPrettyprinter(p: TermsProvider)(term: p.R[p.Term]): String = {
  val isb = IndentedStringBuilder()
  import isb.*

  def ppTerm(term: p.Term): Unit = {
    term match {
      case id: p.Identifier =>
        ppId(id)
      case p.Cap(position) =>
        add(CapKw)
      case p.Select(root, field, position) =>
        ppRecTerm(root)
        add(".").add(field.toString)
      case p.Box(boxed, position) =>
        add(BoxKw).add(" ")
        ppRecTerm(boxed)
      case p.Abs(varId, tpe, body, position) =>
        add(FnKw).add(" (").add(p.str(varId.id))
        add(": ")
        ppType(tpe)
        add(")").incIndent().newLine()
        ppRecTerm(body)
        decIndent()
      case p.RecordLiteral(fields, position) =>
        add("{ ")
        sepList(fields, ", ") { (fld, v) =>
          ppField(fld)
          add(" = ")
          ppRecTerm(v)
        }
        add(" }")
      case p.UnitLiteral(position) =>
        add("()")
      case p.App(callee, arg, position) =>
        ppRecTerm(callee)
        add(" ")
        ppRecTerm(arg)
      case p.Unbox(captureSet, boxed, position) =>
        ppCapt(captureSet)
        add(" ").add(UnboxKw).add(" ")
        ppRecTerm(boxed)
      case p.Let(varId, value, typeAnnotOpt, body, position) =>
        add(LetKw).add(" ")
        ppTerm(varId)
        typeAnnotOpt.foreach { typeAnnot =>
          add(": ")
          ppType(typeAnnot)
        }
        add(" =").incIndent().newLine()
        ppRecTerm(value)
        decIndent().newLine()
        add(InKw).incIndent().newLine()
        ppRecTerm(body)
        decIndent()
      case p.Region(position) =>
        add(RegionKw)
      case p.Deref(ref, position) =>
        add("!")
        ppRecTerm(ref)
      case p.Assign(ref, newVal, position) =>
        ppRecTerm(ref)
        add(" := ")
        ppRecTerm(newVal)
      case p.Ref(regionCap, initVal, position) =>
        ppRecTerm(regionCap)
        add(".").add(RefKw).add(" ")
        ppRecTerm(initVal)
      case p.Module(regionCap, fields, position) =>
        add(ModKw).add("(")
        ppRecTerm(regionCap)
        add(") { ")
        sepList(fields, ", ") { (fld, v) =>
          ppField(fld)
          add(" = ")
          ppRecTerm(v)
        }
        add(" }")
      case p.NamedFieldTree(fieldName, position) =>
        add(fieldName)
      case p.RegFieldTree(position) =>
        add(RegKw)
      case p.TypeTree(shape, captureSet, position) =>
        ppShape(shape)
        captureSet.foreach { cs =>
          add("^")
          ppCapt(cs)
        }
    }
  }

  def ppRecTerm(r: p.R[p.Term]): Unit = p.print(r, ppTerm, add)

  def ppType(tpe: p.TypeTree): Unit = {
    val p.TypeTree(shape, capt, position) = tpe
    ppShape(shape)
    capt.foreach(ppCapt)
  }

  def ppShape(shapeType: p.TypeShapeTree): Unit = shapeType match {
    case p.TopTypeTree(position) =>
      add(TopKw)
    case p.AbsTypeTree(varId, varType, bodyType, position) =>
      add(FnKw).add(" (").add(p.str(varId.id))
      add(": ")
      ppType(varType)
      add(") ")
      ppType(bodyType)
    case p.BoxTypeTree(boxedType, position) =>
      add(BoxKw).add(" ")
      ppType(boxedType)
    case p.UnitTypeTree(position) =>
      add(UnitKw)
    case p.RefTypeTree(referencedType, position) =>
      add(RefKw).add(" ")
      ppShape(referencedType)
    case p.RegTypeTree(position) =>
      add(RegKw)
    case p.RecordTypeTree(selfRef, fieldsInOrder, position) =>
      selfRef.foreach { selfRef =>
        add(SelfKw).add(" ")
        add(p.str(selfRef.id))
        add(" ").add(InKw).add(" ")
      }
      add("{ ")
      sepList(fieldsInOrder, ", ") { (fld, tpe) =>
        ppField(fld)
        add(": ")
        ppType(tpe)
      }
      add(" }")
  }

  def ppCapt(cSet: p.CaptureSetTree): Unit = cSet match {
    case p.NonRootCaptureSet(capturedVarsInOrder, position) =>
      add("{")
      sepList(capturedVarsInOrder, ", ")(ppRecTerm)
      add("}")
    case p.RootCaptureSet(position) =>
      add("{").add(CapKw).add("}")
  }

  def ppField(field: p.FieldTree): Unit = field match {
    case p.NamedFieldTree(fieldName, position) =>
      add(fieldName)
    case p.RegFieldTree(position) =>
      add(RegKw)
  }

  def ppId(id: p.Identifier): Unit = add(p.str(id.id))

  def sepList[T](ls: Seq[T], sep: String)(f: T => Unit): Unit = {
    val iter = ls.iterator
    while (iter.hasNext) {
      f(iter.next())
      if (iter.hasNext) {
        add(", ")
      }
    }
  }

  ppRecTerm(term)
  isb.toString
}
