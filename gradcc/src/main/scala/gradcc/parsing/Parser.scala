package gradcc.parsing

import commons.{Reporter, SimplePhase}
import gradcc.*

class Parser extends SimplePhase[Seq[Token], Ast]("Parser") {

  override protected def runImpl(in: Seq[Token], reporter: Reporter): Ast = {
    val iter = ParsingIterator(in)
    
    ???
  }

}
