package commons.parsing

import commons.Position

trait KindedToken {
  def kind: TokenKind
  def str: String
  def pos: Position
}

trait TokenKind
