package gradcc.parsing

import commons.Position
import commons.parsing.{KindedToken, TokenKind, TreeParser}
import gradcc.lang.{Keyword, Operator}

sealed trait GradCCToken {
  def str: String

  def pos: Position

  override def toString: String = str
}

sealed trait KindedGradCCToken extends GradCCToken, KindedToken

final case class UpperWordToken(override val str: String, override val pos: Position)
  extends KindedGradCCToken {
  override def kind: TokenKind[UpperWordToken] = UpperKind
}

final case class LowerWordToken(override val str: String, override val pos: Position)
  extends KindedGradCCToken {
  override def kind: TokenKind[LowerWordToken] = LowerKind
}

final case class KeywordToken(keyword: Keyword, override val pos: Position)
  extends KindedGradCCToken {
  override val str: String = keyword.str

  override def kind: TokenKind[KeywordToken] = KeywordKind(keyword)
}

final case class OperatorToken(operator: Operator, override val pos: Position)
  extends KindedGradCCToken {
  override val str: Code = operator.str

  override def kind: TokenKind[OperatorToken] = OperatorKind(operator)
}

final case class EndOfFileToken(override val pos: Position) extends KindedGradCCToken {
  override val str: Code = "<end-of-file>"

  override def kind: TokenKind[EndOfFileToken] = EofKind
}

final case class SpaceToken(override val str: String, override val pos: Position) extends GradCCToken

final case class CommentToken(override val str: String, override val pos: Position) extends GradCCToken

final case class ErrorToken(override val str: String, override val pos: Position) extends GradCCToken


sealed trait GradCCTokenKind[T <: KindedGradCCToken](override val tokenClazz: Class[T]) extends TokenKind[T]{
  def parser: TreeParser[KindedGradCCToken, T] = p[KindedGradCCToken]
}

case object UpperKind extends GradCCTokenKind(classOf[UpperWordToken]) {
  override def toString: String = "type identifier"
}

case object LowerKind extends GradCCTokenKind(classOf[LowerWordToken]) {
  override def toString: String = "lowercase identifier"
}

final case class KeywordKind(kw: Keyword) extends GradCCTokenKind(classOf[KeywordToken]) {
  override def toString: String = kw.str
}

final case class OperatorKind(op: Operator) extends GradCCTokenKind(classOf[OperatorToken]) {
  override def toString: String = op.str
}

case object EofKind extends GradCCTokenKind(classOf[EndOfFileToken]) {
  override def toString: String = "end of file"
}
