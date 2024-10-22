package gradcc.asts

import gradcc.lang.Type

object AmbiguouslyNamedTerms extends TermsProvider {

  override type VarId = String
  override type R[T <: TermTree] = T

  override def str(varId: String): String = varId

  override def getTerm[T <: AmbiguouslyNamedTerms.TermTree](r: T): T = r

  override def getType[T <: AmbiguouslyNamedTerms.TermTree](r: T): Option[Type] = None

  override val hasTypes: Boolean = false
}
