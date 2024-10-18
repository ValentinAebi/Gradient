package gradcc.prettyprinting

import gradcc.asts.TermsProvider
import gradcc.lang.Keyword.*


def TermsPrettyprinter(p: TermsProvider)(term: p.Term): String = {
  val isb = IndentedStringBuilder()
  import isb.*
  
  def pp(term: p.Ast): Unit = {
    term match {
      case p.Identifier(id, position) =>
        add(p.str(id))
      case p.Cap(position) =>
        add(CapKw)
      case p.Select(root, field, position) =>
        pp(root)
        add(".").add(field.toString)
      case p.Box(boxed, position) =>
        add(BoxKw).add(" ")
        pp(boxed)
      case p.Abs(varId, tpe, body, position) =>
        add(FnKw).add(" (")
        pp(varId)
        add(": ")
        pp(tpe)
        add(")").incIndent().newLine()
        pp(body)
        decIndent()
      case p.RecordLiteral(fields, position) =>
        add("{ ")
        sepList(fields, ", ") { (fld, v) =>
          pp(fld)
          add(" = ")
          pp(v)
        }
        add(" }")
      case p.UnitLiteral(position) =>
        add("()")
      case p.App(callee, arg, position) =>
        pp(callee)
        add(" ")
        pp(arg)
      case p.Unbox(captureSet, boxed, position) =>
        pp(captureSet)
        add(" ").add(UnboxKw).add(" ")
        pp(boxed)
      case p.Let(varId, value, body, position) =>
        add(LetKw).add(" ")
        pp(varId)
        add(" =").incIndent().newLine()
        pp(value)
        decIndent().newLine()
        add(InKw).incIndent().newLine()
        pp(body)
        decIndent()
      case p.Region(position) =>
        add(RegionKw)
      case p.Deref(ref, position) =>
        add("!")
        pp(ref)
      case p.Assign(ref, newVal, position) =>
        pp(ref)
        add(" := ")
        pp(newVal)
      case p.Ref(regionCap, initVal, position) =>
        pp(regionCap)
        add(".").add(RefKw).add(" ")
        pp(initVal)
      case p.Module(regionCap, fields, position) =>
        add(ModKw).add("(")
        pp(regionCap)
        add(") { ")
        sepList(fields, ", ") { (fld, v) =>
          pp(fld)
          add(" = ")
          pp(v)
        }
        add(" }")
      case p.NamedFieldTree(fieldName, position) =>
        add(fieldName)
      case p.RegFieldTree(position) =>
        add(RegKw)
      case p.TypeTree(shape, captureSet, position) =>
        pp(shape)
        captureSet.foreach { cs =>
          add("^")
          pp(cs)
        }
      case p.TopTypeTree(position) =>
        add(TopKw)
      case p.AbsTypeTree(varId, varType, bodyType, position) =>
        add(FnKw).add(" (")
        pp(varId)
        add(": ")
        pp(varType)
        add(") ")
        pp(bodyType)
      case p.BoxTypeTree(boxedType, position) =>
        add(BoxKw).add(" ")
        pp(boxedType)
      case p.UnitTypeTree(position) =>
        add(UnitKw)
      case p.RefTypeTree(referencedType, position) =>
        add(RefKw).add(" ")
        pp(referencedType)
      case p.RegTypeTree(position) =>
        add(RegKw)
      case p.RecordTypeTree(selfRef, fieldsInOrder, position) =>
        selfRef.foreach { selfRef =>
          add(SelfKw).add(" ")
          pp(selfRef)
          add(" ").add(InKw).add(" ")
        }
        add("{ ")
        sepList(fieldsInOrder, ", ") { (fld, tpe) =>
          pp(fld)
          add(": ")
          pp(tpe)
        }
        add(" }")
      case p.NonRootCaptureSet(capturedVarsInOrder, position) =>
        add("{")
        sepList(capturedVarsInOrder, ", ")(pp)
        add("}")
      case p.RootCaptureSet(position) =>
        add("{").add(CapKw).add("}")
    }
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
  
  pp(term)
  isb.toString
}
