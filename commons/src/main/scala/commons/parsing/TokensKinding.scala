package commons.parsing

import commons.Position

trait KindedToken {
  def kind: TokenKind[?]

  def str: String

  def pos: Position
}

trait TokenKind[T <: KindedToken] {

  val tokenClazz: Class[T]

  def matches(tok: KindedToken): Boolean = tokenClazz.isInstance(tok)

  def ifMatchesOrElse[A](tok: KindedToken)(f: T => A)(default: => A): A = {
    if tokenClazz.isInstance(tok) then f(tokenClazz.cast(tok)) else default
  }

  def p[Tok >: T <: KindedToken]: TreeParser[Tok, T] = LeafParser(this)

}
