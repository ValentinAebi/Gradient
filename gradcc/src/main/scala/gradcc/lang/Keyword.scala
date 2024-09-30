package gradcc.lang

enum Keyword(val str: String) {
  case Box extends Keyword("box")
  case Cap extends Keyword("cap")
  case Dep extends Keyword("dep")
  case In extends Keyword("in")
  case Inreg extends Keyword("inreg")
  case Let extends Keyword("let")
  case Mod extends Keyword("mod")
  case Ref extends Keyword("ref")
  case Reg extends Keyword("reg")
  case Region extends Keyword("region")
  case Top extends Keyword("Top")
  case Unit extends Keyword("Unit")
  case Unbox extends Keyword("unbox")
}
