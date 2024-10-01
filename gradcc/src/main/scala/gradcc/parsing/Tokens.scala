package gradcc.parsing

import commons.Position
import gradcc.lang.{Keyword, Operator}

sealed trait Token {
  val str: String
  val pos: Position

  override def toString: String = s"[$str] ($pos)"
}

sealed trait KindedToken(val kind: TokenKind) extends Token

final case class UpperWordToken(override val str: String, override val pos: Position)
  extends KindedToken(UpperKind)

final case class LowerWordToken(override val str: String, override val pos: Position)
  extends KindedToken(LowerKind)

final case class KeywordToken(keyword: Keyword, override val pos: Position)
  extends KindedToken(KeywordKind(keyword)) {
  override val str: String = keyword.str
}

final case class OperatorToken(operator: Operator, override val pos: Position)
  extends KindedToken(OperatorKind(operator)) {
  override val str: Code = operator.str
}

final case class EndOfFileToken(override val pos: Position) extends KindedToken(EofKind) {
  override val str: Code = "<eof>"
}

final case class SpaceToken(override val str: String, override val pos: Position) extends Token

final case class CommentToken(override val str: String, override val pos: Position) extends Token

final case class ErrorToken(override val str: String, override val pos: Position) extends Token


sealed trait TokenKind

case object UpperKind extends TokenKind {
  override def toString: String = "type identifier"
}

case object LowerKind extends TokenKind {
  override def toString: String = "lowercase identifier"
}

final case class KeywordKind(kw: Keyword) extends TokenKind {
  override def toString: String = kw.str
}

final case class OperatorKind(op: Operator) extends TokenKind {
  override def toString: String = op.str
}

case object EofKind extends TokenKind {
  override def toString: String = "end of file"
}
