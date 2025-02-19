package gradcc.phases.prettyprinting

import gradcc.asts.TermsProvider
import gradcc.lang.Keyword.*
import gradcc.lang.Type


def TermsPrettyprinter(
                        p: TermsProvider,
                        forceIgnoreTypes: Boolean = false
                      )(term: p.R[p.TermTree]): String = {
  
  val considerTypes = p.hasTypes && !forceIgnoreTypes
  val isb = IndentedStringBuilder()
  import isb.*

  def ppTerm(term: p.TermTree, optType: Option[Type] = None): Unit = {
    term match {
      case id: p.IdentifierTree =>
        ppId(id)
      case p.CapTree(position) =>
        add(CapKw)
      case p.SelectTree(root, field, position) =>
        ppRecTerm(root)
        add(".")
        ppField(field)
      case p.BrandedPathTree(properPath, position) =>
        add("#")
        ppRecTerm(properPath)
      case p.BoxTree(boxed, position) =>
        add(BoxKw).add(" ")
        ppRecTerm(boxed)
      case p.AbsTree(varId, tpe, body, position) =>
        add(FnKw).add(" (").add(p.str(varId.id))
        add(": ")
        ppType(tpe)
        add(")")
        p.getType(body).foreach { bodyType =>
          addIfConsiderTypes(s" -> $bodyType")
        }
        incIndent().newLine()
        ppRecTerm(body)
        decIndent()
      case p.RecordLiteralTree(fields, position) =>
        addFieldsList(fields)
      case p.UnitLiteralTree(position) =>
        add("()")
      case p.AppTree(callee, arg, position) =>
        addIfConsiderTypes("(")
        ppRecTerm(callee)
        add(" ")
        ppRecTerm(arg)
        addIfConsiderTypes(")" + typeAnnot(optType))
      case p.UnboxTree(captureSet, boxed, position) =>
        ppCapt(captureSet)
        add(" ").add(UnboxKw).add(" ")
        ppRecTerm(boxed)
      case p.LetTree(varId, value, typeAnnotOpt, body, position) =>
        add(LetKw).add(" ")
        ppTerm(varId)
        typeAnnotOpt.foreach { typeAnnot =>
          add(": ")
          ppType(typeAnnot)
        }
        if (typeAnnotOpt.isEmpty){
          addIfConsiderTypes(typeAnnot(p.getType(value)))
        }
        add(" =").incIndent().newLine()
        ppRecTerm(value)
        decIndent().newLine()
        add(InKw).incIndent().newLine()
        ppRecTerm(body)
        decIndent()
      case p.RegionTree(position) =>
        add(RegionKw)
      case p.DerefTree(ref, position) =>
        add("!")
        ppRecTerm(ref)
      case p.AssignTree(ref, newVal, position) =>
        ppRecTerm(ref)
        add(" := ")
        ppRecTerm(newVal)
      case p.RefTree(regionCap, initVal, position) =>
        ppRecTerm(regionCap)
        add(".").add(RefKw).add(" ")
        ppRecTerm(initVal)
      case p.ModuleTree(regionCap, fields, position) =>
        add(ModKw).add("(")
        ppRecTerm(regionCap)
        add(") ")
        addFieldsList(fields)
      case p.EnclosureTree(permissions, tpe, body, position) =>
        add(EnclKw).add("[")
        ppCapt(permissions)
        add("][")
        ppType(tpe)
        add("]")
        incIndent().newLine()
        ppRecTerm(body)
        decIndent()
      case p.ObscurTree(obscured, varId, body, position) =>
        add(ObscurKw).add(" ")
        ppRecTerm(obscured)
        add(" ").add(AsKw).add(" ")
        ppTerm(varId)
        add(" ").add(InKw).incIndent().newLine()
        ppRecTerm(body)
        decIndent()
    }
  }

  def ppRecTerm(r: p.R[p.TermTree]): Unit = {
    val term = p.getTerm(r)
    ppTerm(term, p.getType(r))
  }

  def ppType(tpe: p.TypeTree): Unit = {
    val p.TypeTree(shape, capt, position) = tpe

    def addCaptIfAny(): Unit = capt.foreach { cp =>
      add("^")
      ppCapt(cp)
    }

    shape match {
      case p.TopShapeTree(position) =>
        add(TopKw)
        addCaptIfAny()
      case p.AbsShapeTree(varId, varType, bodyType, position) =>
        add("(").add(p.str(varId.id)).add(" : ")
        ppType(varType)
        add(") ->")
        capt.foreach(ppCapt)
        add(" ")
        ppType(bodyType)
      case p.BoxShapeTree(boxedType, position) =>
        add(BoxKw).add(" ")
        ppType(boxedType)
        addCaptIfAny()
      case p.UnitShapeTree(position) =>
        add(UnitKw)
        addCaptIfAny()
      case p.RefShapeTree(referencedType, position) =>
        add(RefKw.withFirstUppercase).add(" ")
        ppType(p.TypeTree(referencedType, capt, referencedType.position))
        addCaptIfAny()
      case p.RegShapeTree(position) =>
        add(RegKw.withFirstUppercase)
        addCaptIfAny()
      case p.RecordShapeTree(selfRef, fieldsInOrder, position) =>
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
        addCaptIfAny()
    }
  }

  def ppCapt(cSet: p.CaptureDescriptorTree): Unit = cSet match {
    case p.NonRootCaptureSetTree(capturedVarsInOrder, position) =>
      add("{")
      sepList(capturedVarsInOrder, ", ")(ppRecTerm)
      add("}")
    case p.RootCaptureSetTree(position) =>
      add("{").add(CapKw).add("}")
    case p.BrandDescriptorTree(position) =>
      add("#")
  }

  def ppField(field: p.FieldTree): Unit = field match {
    case p.NamedFieldTree(fieldName, position) =>
      add(fieldName)
    case p.RegFieldTree(position) =>
      add(RegKw)
  }

  def ppId(id: p.IdentifierTree): Unit = add(p.str(id.id))

  def typeAnnot(tpe: Option[Type]): String = {
    val typeDescr = tpe.map(_.toString).getOrElse("??")
    s" : $typeDescr"
  }

  inline def addIfConsiderTypes(inline s: String): Unit = {
    if (considerTypes) {
      add(yellow(s))
    }
  }

  def addFieldsList(fields: Seq[(p.FieldTree, p.R[p.StablePathTree])]): Unit = {
    val multiline = considerTypes && fields.size > 1
    add("{")
    if (multiline) {
      incIndent().newLine()
    } else {
      add(" ")
    }
    val sep = if multiline then ",\n" else ", "
    sepList(fields, sep) { (fld, v) =>
      ppField(fld)
      addIfConsiderTypes(typeAnnot(p.getType(v)))
      add(" = ")
      ppRecTerm(v)
    }
    if (multiline) {
      decIndent().newLine()
    } else {
      add(" ")
    }
    add("}")
  }

  def sepList[T](ls: Seq[T], sep: String)(f: T => Unit): Unit = {
    val iter = ls.iterator
    while (iter.hasNext) {
      f(iter.next())
      if (iter.hasNext) {
        add(sep)
      }
    }
  }

  ppRecTerm(term)
  addIfConsiderTypes("\n" + typeAnnot(p.getType(term)))
  isb.toString
}

private val yellowColorCode: String = "\u001B[33m"
private val cyanColorCode: String = "\u001B[36m"
private val resetColorCode: String = "\u001B[0m"

private def yellow(str: String): String = yellowColorCode + str + resetColorCode
private def cyan(str: String): String = cyanColorCode + str + resetColorCode
