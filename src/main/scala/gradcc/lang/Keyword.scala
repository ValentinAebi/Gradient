package gradcc.lang

enum Keyword(val str: String) {
  case BoxKw extends Keyword("box")
  case CapKw extends Keyword("cap")
  case FnKw extends Keyword("fn")
  case InKw extends Keyword("in")
  case LetKw extends Keyword("let")
  case ModKw extends Keyword("mod")
  case RefKw extends Keyword("ref")
  case RegKw extends Keyword("reg")
  case RegionKw extends Keyword("region")
  case SelfKw extends Keyword("self")
  case TopKw extends Keyword("top")
  case UnboxKw extends Keyword("unbox")
  case UnitKw extends Keyword("Unit")
  case Using extends Keyword("using")

  override def toString: String = str
  
  def withFirstUppercase: String = str.head.toUpper + str.tail
  
}
