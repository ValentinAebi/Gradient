package gradcc.asts
import gradcc.lang.Type

object UniquelyNamedTerms extends UidTermsProvider {
  
  override type R[T <: TermTree] = T

  override def getTerm[T <: UniquelyNamedTerms.TermTree](r: T): T = r

  override def getType[T <: UniquelyNamedTerms.TermTree](r: T): Option[Type] = None

  override val hasTypes: Boolean = false
  
}

final case class UniqueVarId(varName: String, idx: Int){

  def internalName: String = s"$varName#$idx"

  def fullDescr: String = s"$varName (internally $internalName)"
  
  override def toString: String = internalName
  
}
