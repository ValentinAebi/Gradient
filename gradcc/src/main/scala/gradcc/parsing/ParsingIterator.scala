package gradcc.parsing

import commons.Reporter

class ParsingIterator(tokens: Seq[Token]) {
  require(tokens.nonEmpty)

  private val nTokens: Int = tokens.size
  private var remTokens: List[KindedToken] = tokens.flatMap {
    case errorToken: ErrorToken =>
      throw new AssertionError(s"error tokens are not expected at this point ($errorToken)")
    case tok: KindedToken => Some(tok)
    case _ => None
  }.toList

  def current: KindedToken = remTokens.head

  def canMove: Boolean = remTokens.tail.nonEmpty

  def move(): Unit = {
    if (!canMove) {
      throw IllegalStateException()
    }
    remTokens = remTokens.tail
  }

  def startsWith(tokenKind: TokenKind): Boolean = (current.kind == tokenKind)

  def consumeSeq(kinds: Seq[TokenKind], reporter: Reporter): Seq[KindedToken] = {
    require(kinds.nonEmpty)
    val tokens = new Array[KindedToken](kinds.size)
    for ((kind, idx) <- kinds.zipWithIndex) {
      if (!startsWith(kind)) {
        reporter.fatal(s"expected $kind, but was ${current.kind}", current.pos)
      }
      tokens(idx) = current
      move()
    }
    tokens
  }

  def expectEnded(reporter: Reporter): Unit = {
    if (remTokens.size > 1) {
      reporter.fatal("unmatched tokens after successfully parsed expression", current.pos)
    }
  }

}
