package gradcc.asts

object UniquelyNamedTerms extends UidTermsProvider {
  
  override type R[T <: Term] = T

  override def getTerm[T <: UniquelyNamedTerms.Term](r: T): T = r

  override def print[T <: Term](r: T, printT: T => Unit, printStr: String => Unit): Unit =
    printT(r)
}

final case class UniqueVarId(varName: String, idx: Int){

  def internalName: String = s"$varName#$idx"

  def fullDescr: String = s"$varName (internally $internalName)"
  
  override def toString: String = internalName
  
}
