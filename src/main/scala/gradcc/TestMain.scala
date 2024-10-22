package gradcc

import gradcc.*
import gradcc.asts.TypedTerms
import gradcc.phases.{Fatal, NonFatal, Success}
import gradcc.phases.parsing.{ParserPhase, ScannerPhase}
import gradcc.phases.prettyprinting.PrettyprinterPhase
import gradcc.phases.renaming.RenamerPhase
import gradcc.phases.typechecking.TypeCheckerPhase
import gradcc.reporting.Reporter

import scala.io.Source
import scala.util.Using

@main def main(path: String): Unit = {
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
      case NonFatal(faultyValue) =>
        s" !!! There were error(s) !!!\n\n$faultyValue"
      case Fatal(fatalErrorException) =>
        fatalErrorException.toString
  )
  println()
  if (reporter.somethingReported) {
    println(reporter.getStringReport)
  } else {
    println("No error found")
  }
}
