package gradcc.asts

import gradcc.lang.Type

object TypedTerms extends UidTermsProvider {

  override type R[+T <: TermTree] = TypedTerm[T]

  override def getTerm[T <: TypedTerms.TermTree](r: TypedTerm[T]): T = r.term

  override def getType[T <: TypedTerms.TermTree](r: TypedTerm[T]): Option[Type] = r.tpe

  override val hasTypes: Boolean = true

}

final case class TypedTerm[+T <: TypedTerms.TermTree](term: T, tpe: Option[Type])
