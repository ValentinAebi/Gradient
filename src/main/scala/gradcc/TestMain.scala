package gradcc

import gradcc.*
import gradcc.asts.TypedTerms
import gradcc.parsing.{ParserPhase, ScannerPhase}
import gradcc.prettyprinting.PrettyprinterPhase
import gradcc.renaming.RenamerPhase
import gradcc.typechecking.TypeCheckerPhase

import scala.io.Source
import scala.util.Using

@main def main(): Unit = {

  val path = "examples/ex1.gradcc"

  val str = Using(Source.fromFile(path))(_.getLines().mkString("\n")).get
  val reporter = new Reporter()
  val pipeline =
    ScannerPhase()
      .andThen(ParserPhase())
      .andThen(RenamerPhase())
      .andThen(TypeCheckerPhase())
      .andThen(PrettyprinterPhase(TypedTerms))
  val res = pipeline.run((str, path), reporter)
  println(
    res match
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
