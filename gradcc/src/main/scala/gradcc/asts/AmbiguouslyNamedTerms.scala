package gradcc.asts

object AmbiguouslyNamedTerms extends TermsProvider {

  override type VarId = String

  override def str(varId: String): String = varId
}
