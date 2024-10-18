package gradcc.asts

import gradcc.lang.Type

object TypedTerms extends TermsProvider {

  override type VarId = UniqueVarId

  override type R[+T <: Term] = TypedTerm[T]

  override def str(varId: UniqueVarId): String = varId.toString

  override def print[T <: TypedTerms.Term](r: TypedTerm[T], printT: T => Unit, printStr: String => Unit): Unit = {
    val TypedTerm(term, tpe) = r
    printT(term)
    printStr(s" : $tpe")
  }

  final case class TypedTerm[+T <: Term](term: T, tpe: Option[Type])

}
