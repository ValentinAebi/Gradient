package gradcc.lang

enum Keyword(val str: String) {
  case BoxKw extends Keyword("box")
  case CapKw extends Keyword("cap")
  case DepKw extends Keyword("dep")
  case FnKw extends Keyword("fn")
  case InKw extends Keyword("in")
  case LetKw extends Keyword("let")
  case ModKw extends Keyword("mod")
  case RefKw extends Keyword("ref")
  case RegKw extends Keyword("reg")
  case RegionKw extends Keyword("region")
  case SelfKw extends Keyword("self")
  case TopKw extends Keyword("Top")
  case UnitKw extends Keyword("Unit")
  case UnboxKw extends Keyword("unbox")

  override def toString: String = str
}
