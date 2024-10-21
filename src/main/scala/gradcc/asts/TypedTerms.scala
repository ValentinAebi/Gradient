package gradcc.asts

import gradcc.lang.Type

object TypedTerms extends UidTermsProvider {

  override type R[+T <: Term] = TypedTerm[T]

  override def getTerm[T <: TypedTerms.Term](r: TypedTerm[T]): T = r.term

  override def getType[T <: TypedTerms.Term](r: TypedTerm[T]): Option[Type] = r.tpe

  override val hasTypes: Boolean = true

}

final case class TypedTerm[+T <: TypedTerms.Term](term: T, tpe: Option[Type])
