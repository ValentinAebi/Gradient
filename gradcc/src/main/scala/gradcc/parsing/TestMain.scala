package gradcc.parsing

import gradcc.asts.UniquelyNamedTerms
import gradcc.prettyprinting.{PrettyprinterPhase, TermsPrettyprinter, TypesPrettyprinter}
import gradcc.renaming.RenamerPhase
import gradcc.typechecking.TypeCheckerPhase
import gradcc.*
import gradcc.lang.Type

@main def main(): Unit = {

  enum Action {
    case Prettyprint, Typecheck
  }

  ///////////////////////////////////////////////////////////
  val action = Action.Typecheck
  val str =
    """
      |fn (x: Reg)
      |  let r = x in
      |     let x = fn (x: Unit, y: Reg^{x}, r: Reg^, g: fn (x: Unit) Reg) g x
      |     in mod (x) { a = x, b = r }
      |""".stripMargin
  ///////////////////////////////////////////////////////////

  //  val str =
  //    """
  //      |let newLogger = fn (fs: Fs^)
  //      |  let r = region in
  //      |  let _log = fn (msg : String ) = ... in
  //      |  mod (r) { log = _log }
  //      |in
  //      |
  //      |let newMain = fn (fs: Fs^)(net: Net^)
  //      |  let r = region in
  //      |  let _main = fn (u: Unit)
  //      |    let logger = newLogger fs in ...
  //      |  in mod (r) { main = _main }
  //      |in
  //      |
  //      |// initialize Main & run the program
  //      |let main = newMain fs net in
  //      |main.main()
  //      |""".stripMargin
  val reporter = new Reporter()
  val pipeline: Phase[(Code, Filename), String | Map[UniquelyNamedTerms.Term, Type]] =
    ScannerPhase()
      .andThen(ParserPhase())
      .andThen(RenamerPhase())
      .andThen(
        if action == Action.Prettyprint then PrettyprinterPhase(UniquelyNamedTerms)
        else TypeCheckerPhase()
      )
  val res = pipeline.run((str, "Example.gradcc"), reporter)
  println(
    res match
      case Success(typesAssignment: Map[UniquelyNamedTerms.Term, Type]) => {
        val termsPp: UniquelyNamedTerms.Term => String = TermsPrettyprinter(UniquelyNamedTerms)
        val typesPp = TypesPrettyprinter
        typesAssignment.map((term, tpe) => s"${termsPp(term)} : ${typesPp(tpe)}").mkString("\n\n")
      }
      case Success(value) =>
        s"\n$value"
      case Fatal(fatalErrorException) =>
        s"Fatal error: ${fatalErrorException.msg} at ${fatalErrorException.pos.getOrElse("??")}"
      case NonFatal =>
        "Failure: non-fatal error(s) terminated the pipeline"
  )
  println()
  if (reporter.somethingToReport) {
    println(reporter.getStringReport)
  } else {
    println("No error found")
  }
}
