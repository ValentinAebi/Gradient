package gradcc.asts

object UniquelyNamedTerms extends TermsProvider {

  override type VarId = UniqueVarId

  override def str(varId: UniqueVarId): String = varId.toString
}

final case class UniqueVarId(varName: String, idx: Int){
  override def toString: String = s"$varName#$idx"
}
