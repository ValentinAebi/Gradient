package gradcc.parsing

import commons.Position
import gradcc.lang.{Conventions, Keyword, Operator}

sealed trait Token {
  val str: String
  val pos: Position

  override def toString: String = str
}

final case class UpperWordToken(override val str: String, override val pos: Position) extends Token

final case class LowerWordToken(override val str: String, override val pos: Position) extends Token

final case class KeywordToken(keyword: Keyword, override val pos: Position) extends Token {
  override val str: String = keyword.str
}

final case class OperatorToken(operator: Operator, override val pos: Position) extends Token {
  override val str: Code = operator.str
}

final case class SpaceToken(override val str: String, override val pos: Position) extends Token

final case class CommentToken(override val str: String, override val pos: Position) extends Token

final case class ErrorToken(override val str: String, override val pos: Position) extends Token
