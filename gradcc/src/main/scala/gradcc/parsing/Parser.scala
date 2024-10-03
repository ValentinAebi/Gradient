package gradcc.parsing

import commons.parsing.*
import commons.{Position, Reporter, SimplePhase}
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
    val syntax = kw(LetKw) ~ rep(lower ~ (op(Equal) | op(Colon)) ~ lower) ~ eof
    syntax.consume(iter, throw new AssertionError("should never be invoked"), reporter) match {
      case let ~ ls ~ () => {
        println(ls.map { case l ~ op ~ r => s"'$l' '$op' '$r'" }.mkString("\n"))
      }
    }
    null
  }

  private def lower = LowerKind.parser

  private def kw(kw: Keyword) = KeywordKind(kw).parser

  private def op(op: Operator) = OperatorKind(op).parser

  private def eof = EofParser[KindedGradCCToken](EofKind)

  private def pos(using Position): Position = summon

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
