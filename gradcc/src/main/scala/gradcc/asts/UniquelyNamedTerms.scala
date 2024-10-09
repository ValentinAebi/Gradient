package gradcc.asts

object UniquelyNamedTerms extends TermsProvider {

  override type VarId = UniqueVarId

}

final case class UniqueVarId(varName: String, idx: Int)
