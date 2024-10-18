package gradcc

import gradcc.*
import gradcc.asts.UniquelyNamedTerms
import gradcc.lang.Type
import gradcc.parsing.{Code, Filename, ParserPhase, ScannerPhase}
import gradcc.prettyprinting.PrettyprinterPhase
import gradcc.renaming.RenamerPhase
import gradcc.typechecking.TypeCheckerPhase

import scala.io.Source
import scala.util.Using

@main def main(): Unit = {

  enum Action {
    case Prettyprint, Typecheck
  }

  ///////////////////////////////////////////////////////////
  val action = Action.Typecheck
  val path = "examples/ex5.gradcc"
  ///////////////////////////////////////////////////////////

  val str = Using(Source.fromFile(path))(_.getLines().mkString("\n")).get
  val reporter = new Reporter()
  val pipeline: Phase[(Code, Filename), String | Map[UniquelyNamedTerms.Term, Type]] =
    ScannerPhase()
      .andThen(ParserPhase())
      .andThen(RenamerPhase())
      .andThen(
        if action == Action.Prettyprint
        then PrettyprinterPhase(UniquelyNamedTerms)
        else TypeCheckerPhase()
      )
  val res = pipeline.run((str, path), reporter)
  println(
    res match
      case Success(typesAssignment: Map[UniquelyNamedTerms.Term, Type]) => {
        val (_, resType) = typesAssignment.maxBy(_._1.toString.length)
        "\n" + colorParentheses(resType.toString)
      }
      case Success(value) =>
        s"\n$value"
      case Fatal(fatalErrorException) =>
        fatalErrorException.toString
      case NonFatal =>
        "Failure: non-fatal error(s) terminated the pipeline"
  )
  println()
  if (reporter.somethingReported) {
    println(reporter.getStringReport)
  } else {
    println("No error found")
  }
}

private val resetColorCode: String = "\u001B[0m"
private def mkColor(depth: Int): String = s"\u001B[3${depth % 6 + 1}m"

private def colorParentheses(str: String): String = {
  var depth = 0
  val sb = new StringBuilder()
  for (c <- str) {
    if (c == '(' || c == '{') {
      depth += 1
    }
    if (c == '(' || c == ')' || c == '{' || c == '}') {
      sb.append(mkColor(depth)).append(c).append(resetColorCode)
    } else {
      sb.append(c)
    }
    if (c == ')' || c == '}') {
      depth -= 1
    }
  }
  sb.toString()
}
