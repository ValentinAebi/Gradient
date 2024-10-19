package gradcc.asts

import gradcc.lang.Type

object TypedTerms extends UidTermsProvider {

  override type R[+T <: Term] = TypedTerm[T]

  override def getTerm[T <: TypedTerms.Term](r: TypedTerm[T]): T = r.term

  override def print[T <: TypedTerms.Term](r: TypedTerm[T], printT: T => Unit, printStr: String => Unit): Unit = {
    val TypedTerm(term, tpe) = r
    printT(term)
    printStr(s"$cyanColorCode:${tpe.getOrElse("??")}$resetColorCode")
  }

  private val cyanColorCode: String = "\u001B[36m"
  private val resetColorCode: String = "\u001B[0m"

}

final case class TypedTerm[+T <: TypedTerms.Term](term: T, tpe: Option[Type])
