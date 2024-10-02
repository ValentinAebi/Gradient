import commons.Reporter
import gradcc.parsing.Scanner
import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse}
import org.junit.jupiter.api.Test

class ScannerTests {

  @Test def simpleScannerTest(): Unit = {
    val code =
      """let f =
        |  fn (x: Foo^{t})
        |         bar x y
        |""".stripMargin
    val fakeFilename = "Example.gradcc"
    val scanner = Scanner()
    val reporter = Reporter()
    val actual = scanner.run((code, fakeFilename), reporter).resultOrThrow()
    val expected = List(
      "let" -> (1, 1),
      " " -> (1, 4),
      "f" -> (1, 5),
      " " -> (1, 6),
      "=" -> (1, 7),
      "  " -> (2, 1),
      "fn" -> (2, 3),
      " " -> (2, 5),
      "(" -> (2, 6),
      "x" -> (2, 7),
      ":" -> (2, 8),
      " " -> (2, 9),
      "Foo" -> (2, 10),
      "^" -> (2, 13),
      "{" -> (2, 14),
      "t" -> (2, 15),
      "}" -> (2, 16),
      ")" -> (2, 17),
      "         " -> (3, 1),
      "bar" -> (3, 10),
      " " -> (3, 13),
      "x" -> (3, 14),
      " " -> (3, 15),
      "y" -> (3, 16),
      "<end-of-file>" -> (3, 17)
    )
    val actualIter = actual.iterator
    val expectedIter = expected.iterator
    while (actualIter.hasNext && expectedIter.hasNext) {
      val actualToken = actualIter.next()
      val pos = actualToken.pos
      assertEquals(expectedIter.next(), actualToken.str -> (pos.line, pos.col))
    }
    assertFalse(actualIter.hasNext)
    assertFalse(expectedIter.hasNext)
  }

}
