package gradcc.typechecking

import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*


def substitute(term: Term)(using subst: Map[UniqueVarId, Path]): Term = term match {
  case ident: Identifier =>
    substitute(ident)
  case cap: Cap => cap
  case Select(root, select, position) =>
    Select(substitute(root), select, position)
  case Box(boxed, position) =>
    Box(substitute(boxed), position)
  case Abs(varId, tpe, body, position) =>
    Abs(varId, substitute(tpe), substitute(body), position)
  case RecordLiteral(selfRef, fields, position) => ???
  case UnitLiteral(position) => ???
  case App(callee, arg, position) => ???
  case Unbox(captureSet, boxed, position) => ???
  case Let(varId, value, body, position) => ???
  case Region(position) => ???
  case Deref(ref, position) => ???
  case Assign(ref, newVal, position) => ???
  case Ref(regionCap, initVal, position) => ???
  case Modif(regionCap, fields, position) => ???
}

def substitute(path: Path)(using subst: Map[UniqueVarId, Path]): Path = path match {
  case ident@Identifier(id, position) =>
    subst.getOrElse(id, ident)
  case Select(root, select, position) =>
    Select(substitute(root), select, position)
}

def substitute(typeTree: TypeTree): TypeTree = {
  val TypeTree(shape, captureSet, position) = typeTree
  TypeTree(substitute(shape), captureSet.map(substitute), position)
}

def substitute(typeShapeTree: TypeShapeTree): TypeShapeTree = typeShapeTree match {
  case TopTypeTree(position) => ???
  case AbsTypeTree(varId, varType, bodyType, position) => ???
  case BoxTypeTree(boxedType, position) => ???
  case UnitTypeTree(position) => ???
  case RefTypeTree(referencedType, position) => ???
  case RegTypeTree(position) => ???
  case RecordTypeTree(selfRef, fieldsInOrder, position) => ???
}

def substitute(captureSetTree: CaptureSetTree): CaptureSetTree = captureSetTree match {
  case ExplicitCaptureSetTree(capturedVarsInOrder, position) => ???
  case ImplicitCaptureSetTree(position) => ???
}
