package gradcc.parsing

import scala.collection.immutable.ArraySeq
import scala.util.parsing.input
import scala.util.parsing.input.Reader

sealed trait ParsingReader extends Reader[CoreGradCCToken] {
  override def pos: input.Position = first.pos
}

final case class NonTerminal(first: CoreGradCCToken, rest: Reader[CoreGradCCToken]) extends ParsingReader {
  override def atEnd: Boolean = false
}

case object Terminal extends ParsingReader {
  override def first: CoreGradCCToken = throw UnsupportedOperationException("first of Terminal")
  override def rest: Reader[CoreGradCCToken] = throw UnsupportedOperationException("rest of Terminal")

  override def atEnd: Boolean = true
}

object ParsingReader {

  def from(tokens: ArraySeq[CoreGradCCToken]): ParsingReader = {
    var res: ParsingReader = Terminal
    for (tok <- tokens.reverse) {
      res = NonTerminal(tok, res)
    }
    res
  }

}
