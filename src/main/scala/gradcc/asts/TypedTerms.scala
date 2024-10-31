package gradcc.asts

import gradcc.lang.Type

object TypedTerms extends UidTermsProvider {

  override type R[+T <: TermTree] = TypedTermTree[T]

  override def getTerm[T <: TypedTerms.TermTree](r: TypedTermTree[T]): T = r.term

  override def getType[T <: TypedTerms.TermTree](r: TypedTermTree[T]): Option[Type] = r.tpe

  override val hasTypes: Boolean = true

}

final case class TypedTermTree[+T <: TypedTerms.TermTree](term: T, tpe: Option[Type])
