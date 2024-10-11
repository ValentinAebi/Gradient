package gradcc.prettyprinting

import gradcc.{Reporter, SimplePhase}
import gradcc.asts.TermsProvider
import gradcc.lang.Keyword.*

def PrettyprinterPhase(p: TermsProvider): SimplePhase[p.Term, String] = new SimplePhase[p.Term, String]("Prettyprinter"){

  override protected def runImpl(in: p.Term, reporter: Reporter): String = {
    val isb = new IndentedStringBuilder()
    pp(in)(using isb)
    isb.toString
  }

  private def pp(term: p.Ast)(using isb: IndentedStringBuilder): Unit = {
    import isb.*
    term match {
      case p.Identifier(id, position) =>
        add(p.str(id))
      case p.Cap(position) =>
        add(CapKw)
      case p.Select(root, select, position) =>
        pp(root)
        add(".").add(select)
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
      case p.RecordLiteral(selfRef, fields, position) =>
        selfRef.foreach { selfRef =>
          add(SelfKw).add(" ")
          pp(selfRef)
          add(" ").add(InKw).add(" ")
        }
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
        add(UnboxKw).add(" ")
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
        add(".").add(RefLKw).add(" ")
        pp(initVal)
      case p.Modif(regionCap, fields, position) =>
        add(ModKw).add("(")
        pp(regionCap)
        add(") { ")
        sepList(fields, ", ") { (fld, v) =>
          pp(fld)
          add(" = ")
          pp(v)
        }
        add(" }")
      case p.NamedField(fieldName, position) =>
        add(fieldName)
      case p.Reg(position) =>
        add(RegLKw)
      case p.TypeTree(shape, captureSet, position) =>
        pp(shape)
        captureSet.foreach(pp)
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
        add(RefUKw).add(" ")
        pp(referencedType)
      case p.RegTypeTree(position) =>
        add(RegUKw)
      case p.RecordTypeTree(selfRef, fieldsInOrder, position) =>
        add("{ ")
        sepList(fieldsInOrder, ", ") { (fld, v) =>
          pp(fld)
          add(": ")
          pp(v)
        }
        add(" }")
      case p.ExplicitCaptureSetTree(capturedVarsInOrder, position) =>
        add("^{ ")
        sepList(capturedVarsInOrder, ", ")(pp)
        add(" }")
      case p.ImplicitCaptureSetTree(position) =>
        add("^{ ").add(CapKw).add(" }")
    }
  }

  private def sepList[T](ls: Seq[T], sep: String)(f: IndentedStringBuilder ?=> T => Unit)(using isb: IndentedStringBuilder): Unit = {
    val iter = ls.iterator
    while (iter.hasNext) {
      f(iter.next())
      if (iter.hasNext) {
        isb.add(", ")
      }
    }
  }


}