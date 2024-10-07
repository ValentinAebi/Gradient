package gradcc.parsing

import commons.{Fatal, NonFatal, Reporter, Success}

@main def main(): Unit = {
  val reporter = new Reporter()
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
  val str = "let g = fn (x: Unit, y: Reg^{x, a}, foo: Reg^) f x x in mod (r) { a = x, b = y }"
  val pipeline = new Scanner().andThen(new ParserPhase())
  val res = pipeline.run((str, "Example.gradcc"), reporter)
  println(
    res match
      case Success(value) => s"Success: $value"
      case Fatal(fatalErrorException) => s"Fatal error: ${fatalErrorException.msg} at ${fatalErrorException.pos.getOrElse("??")}"
      case NonFatal => "Non-fatal error(s) terminated the pipeline (see reporter)"
  )
}
