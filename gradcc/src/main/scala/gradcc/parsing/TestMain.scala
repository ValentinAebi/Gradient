package gradcc.parsing

import commons.Reporter

@main def main(): Unit = {
  val scanner = new Scanner()
  val reporter = new Reporter()
  val str =
    """
      |let newLogger = fn (fs: Fs^)
      |  let r = region in
      |  let _log = fn (msg : String ) = ... in
      |  mod (r) { log = _log }
      |in
      |
      |let newMain = fn (fs: Fs^)(net: Net^)
      |  let r = region in
      |  let _main = fn (u: Unit)
      |    let logger = newLogger fs in ...
      |  in mod (r) { main = _main }
      |in
      |
      |// initialize Main & run the program
      |let main = newMain fs net in
      |main.main()
      |""".stripMargin
  val tokens = scanner.run((str, "example.scala"), reporter).resultOrThrow()
  for (token <- tokens) {
    println(s"${token.pos} : [$token] ${token.getClass.getSimpleName}")
  }
}
