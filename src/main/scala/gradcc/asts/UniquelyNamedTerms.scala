package gradcc.asts

object UniquelyNamedTerms extends TermsProvider {

  override type VarId = UniqueVarId

  override def str(varId: UniqueVarId): String = varId.toString
}

final case class UniqueVarId(varName: String, idx: Int){

  def internalName: String = s"$varName#$idx"

  def fullDescr: String = s"$varName (internally $internalName)"
  
  override def toString: String = internalName
  
}
