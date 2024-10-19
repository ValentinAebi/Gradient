package gradcc.asts

object AmbiguouslyNamedTerms extends TermsProvider {

  override type VarId = String
  override type R[T <: Term] = T

  override def str(varId: String): String = varId

  override def getTerm[T <: AmbiguouslyNamedTerms.Term](r: T): T = r

  override def print[T <: Term](r: T, printT: T => Unit, printStr: String => Unit): Unit =
    printT(r)
}
