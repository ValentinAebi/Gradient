package gradcc.lang

enum Keyword(val str: String) {
  case BoxKw extends Keyword("box")
  case CapKw extends Keyword("cap")
  case FnKw extends Keyword("fn")
  case InKw extends Keyword("in")
  case LetKw extends Keyword("let")
  case ModKw extends Keyword("mod")
  case RefLKw extends Keyword("ref")
  case RefUKw extends Keyword("Ref")
  case RegLKw extends Keyword("reg")
  case RegUKw extends Keyword("Reg")
  case RegionKw extends Keyword("region")
  case SelfKw extends Keyword("self")
  case TopKw extends Keyword("Top")
  case UnboxKw extends Keyword("unbox")
  case UnitKw extends Keyword("Unit")
  case Using extends Keyword("using")

  override def toString: String = str
}
