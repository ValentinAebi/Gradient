import gradcc.asts.AmbiguouslyNamedTerms.TermTree
import gradcc.parsing.{ParserPhase, ScannerPhase}
import gradcc.{PhaseResult, Reporter}
import org.junit.Assert.assertTrue
import org.junit.Test

import java.nio.file.{Files, Path}

class ParseExamplesTests {

  @Test
  def parseExamplesTest(): Unit = {
    val reporter = Reporter()
    val results =
      Files.list(Path.of("./examples")).map(parseExampleTest(reporter))
        .toArray(new Array[PhaseResult[TermTree]](_))
    val isSuccess = !reporter.errorFlagIsRaised && results.forall(_.isSuccess)
    assertTrue("\n" + reporter.getStringReport, isSuccess)
  }

  private def parseExampleTest(reporter: Reporter)(path: Path): PhaseResult[TermTree] = {
    val code = Files.readAllLines(path).toArray.mkString("\n")
    val frontend = ScannerPhase().andThen(ParserPhase())
    frontend.run((code, path.toString), reporter)
  }

}
