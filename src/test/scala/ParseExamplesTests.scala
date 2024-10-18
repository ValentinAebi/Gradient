import gradcc.asts.AmbiguouslyNamedTerms.Term
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
        .toArray(new Array[PhaseResult[Term]](_))
    val isSuccess = !reporter.compilerMustStop() && results.forall(_.isSuccess)
    assertTrue("\n" + reporter.getStringReport, isSuccess)
  }

  private def parseExampleTest(reporter: Reporter)(path: Path): PhaseResult[Term] = {
    val code = Files.readAllLines(path).toArray.mkString("\n")
    val frontend = ScannerPhase().andThen(ParserPhase())
    frontend.run((code, path.toString), reporter)
  }

}
