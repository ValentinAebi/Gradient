package gradcc.phases.parsing

import gradcc.lang.{Keyword, Operator}
import gradcc.reporting.Position

sealed trait GradCCToken {
  def str: String

  def pos: Position

  override def toString: String = str
}

sealed trait CoreGradCCToken extends GradCCToken

final case class UpperWordToken(override val str: String, override val pos: Position) extends CoreGradCCToken

final case class LowerWordToken(override val str: String, override val pos: Position)
  extends CoreGradCCToken

final case class KeywordToken(keyword: Keyword, override val pos: Position)
  extends CoreGradCCToken {
  override val str: String = keyword.str
}

final case class OperatorToken(operator: Operator, override val pos: Position)
  extends CoreGradCCToken {
  override val str: Code = operator.str
}

final case class EndOfFileToken(override val pos: Position) extends CoreGradCCToken {
  override val str: Code = "<end-of-file>"
}

final case class SpaceToken(override val str: String, override val pos: Position) extends GradCCToken

final case class CommentToken(override val str: String, override val pos: Position) extends GradCCToken

final case class ErrorToken(override val str: String, override val pos: Position) extends GradCCToken

