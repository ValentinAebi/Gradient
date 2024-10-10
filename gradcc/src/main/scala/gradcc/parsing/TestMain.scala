package gradcc.parsing

import commons.{Fatal, NonFatal, Reporter, Success}
import gradcc.asts.{AmbiguouslyNamedTerms, UniquelyNamedTerms}
import gradcc.prettyprinting.Prettyprinter
import gradcc.renaming.RenamerPhase

@main def main(): Unit = {

  ///////////////////////////////////////////////////////////
  val performRenaming = true
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
  val end =
    if performRenaming then RenamerPhase().andThen(Prettyprinter(UniquelyNamedTerms))
    else Prettyprinter(AmbiguouslyNamedTerms)
  val pipeline = ScannerPhase().andThen(ParserPhase()).andThen(end)
  val res = pipeline.run((str, "Example.gradcc"), reporter)
  println(
    res match
      case Success(value) =>
        s"\n$value"
      case Fatal(fatalErrorException) =>
        s"Fatal error: ${fatalErrorException.msg} at ${fatalErrorException.pos.getOrElse("??")}"
      case NonFatal =>
        "Failure: non-fatal error(s) terminated the pipeline"
  )
  if (reporter.somethingToReport) {
    println()
    println(reporter.getStringReport)
  }
}
