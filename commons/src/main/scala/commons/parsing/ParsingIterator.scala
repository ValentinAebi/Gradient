package commons.parsing

import commons.Reporter

import scala.collection.immutable.ArraySeq

class ParsingIterator[+Tok <: KindedToken](tokens: ArraySeq[Tok]) {
  require(tokens.nonEmpty)
  
  private var currIdx = 0

  def current: Tok = tokens(currIdx)

  def canMove: Boolean = currIdx < tokens.size - 1

  def move(reporter: Reporter): Unit = {
    val pos = current.pos
    if (!canMove) {
      // FIXME pos is the start position of the previous token
      reporter.fatal("unexpected end of tokens flow", pos)
    }
    currIdx += 1
  }

}
