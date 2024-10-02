package gradcc.parsing

import commons.parsing.{LeafParser, ParsingIterator, TokenKind, TreeParser, rep, ~}
import commons.{Reporter, SimplePhase}
import gradcc.*
import gradcc.lang.Keyword.LetKw
import gradcc.lang.Operator.{Colon, Equal}
import gradcc.lang.{Keyword, Operator}

import scala.collection.mutable.ListBuffer

class Parser extends SimplePhase[Seq[GradCCToken], Ast]("Parser") {

  override protected def runImpl(in: Seq[GradCCToken], reporter: Reporter): Ast = {
    val interestingTokens = filterIsKindedToken(in)
    val iter = ParsingIterator[KindedGradCCToken](interestingTokens)

    // TODO remove this (debug example)
    val syntax = kw(LetKw) ~ rep(lower ~ (op(Equal) | op(Colon)) ~ lower) ~ EofParser
    syntax.consume(iter, throw new AssertionError("should never be invoked"), reporter) match {
      case let ~ ls ~ () => {
        println(ls.map { case l ~ op ~ r => s"'$l' '$op' '$r'" }.mkString("\n"))
      }
    }
    null
  }

  private def lower = LeafParser(LowerKind){
    (tok: KindedGradCCToken) => tok.str
  }

  private def kw(kw: Keyword) = LeafParser(KeywordKind(kw)){
    (tok: KindedGradCCToken) => kw
  }

  private def op(op: Operator) = LeafParser(OperatorKind(op)){
    (tok: KindedGradCCToken) => op
  }

  private object EofParser extends TreeParser[KindedGradCCToken, Unit] {
    override val descr: String = "<eof>"

    override def admits(iterator: ParsingIterator[KindedGradCCToken], nextAdmits: => Boolean): Boolean =
      iterator.current.kind == EofKind

    override def consume(iterator: ParsingIterator[KindedGradCCToken], nextAdmits: => Boolean, reporter: Reporter): Unit = {
      assert(iterator.current.kind == EofKind && !iterator.canMove)
    }

    override def first: Set[TokenKind] = Set(EofKind)

    override def mayBeTransparent: Boolean = false
  }

  private def filterIsKindedToken(tokens: Seq[GradCCToken]): Seq[KindedGradCCToken] = {
    val b = ListBuffer.empty[KindedGradCCToken]
    tokens foreach {
      case kindedGradCCToken: KindedGradCCToken =>
        b.addOne(kindedGradCCToken)
      case _ => ()
    }
    b.toSeq
  }

}
