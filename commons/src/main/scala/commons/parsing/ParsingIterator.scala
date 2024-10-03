package commons.parsing

import commons.Reporter

class ParsingIterator[Tok <: KindedToken](tokens: Seq[Tok]) {
  require(tokens.nonEmpty)

  private val nTokens: Int = tokens.size
  private var remTokens: List[Tok] = tokens.toList

  def current: Tok = remTokens.head

  def canMove: Boolean = remTokens.tail.nonEmpty

  def move(reporter: Reporter): Unit = {
    val pos = current.pos
    if (!canMove) {
      // FIXME pos is the start position of the previous token
      reporter.fatal("unexpected end of tokens flow", pos)
    }
    remTokens = remTokens.tail
  }

}
