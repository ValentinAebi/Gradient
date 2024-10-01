package gradcc.lang

enum Operator(val str: String) {
  case Dot extends Operator(".")
  case Comma extends Operator(",")
  case Colon extends Operator(":")
  case OpenParenth extends Operator("(")
  case CloseParenth extends Operator(")")
  case OpenBrace extends Operator("{")
  case CloseBrace extends Operator("}")
  case Equal extends Operator("=")
  case ColumnEqual extends Operator(":=")
  case Hat extends Operator("^")

  override def toString: String = str
}