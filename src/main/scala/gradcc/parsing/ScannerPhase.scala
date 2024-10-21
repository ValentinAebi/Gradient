package gradcc.parsing

import gradcc.lang.{Conventions, Keyword, Operator}
import gradcc.parsing.ScannerPhase.{lowerWordRegex, upperWordRegex}
import gradcc.{Position, Reporter, SimplePhase, parsing}

import scala.util.matching.Regex

type Code = String
type Filename = String


final class ScannerPhase extends SimplePhase[(Code, Filename), Seq[GradCCToken]]("Scanner") {

  private val commentMatcher: Matcher = (str: String, pos: Position) =>
    if str.startsWith(Conventions.commentMarker) then Some(CommentToken(str, pos)) else None

  private def operatorMatcher(operator: Operator): Matcher = {
    (str: String, pos: Position) => if str.startsWith(operator.str) then Some(OperatorToken(operator, pos)) else None
  }

  private def keywordMatcher(keyword: Keyword): Matcher = {
    val lowerKw = keyword.str.toLowerCase
    (str: String, pos: Position) =>
      val lowerStr = str.toLowerCase
      if lowerStr.startsWith(lowerKw) && hasNoMatchLongerThanKeyword(lowerKw, lowerStr)
      then Some(KeywordToken(keyword, pos))
      else None
  }

  private def hasNoMatchLongerThanKeyword(lowerKw: String, str: Code) = {
    lowerWordRegex.findPrefixOf(str.replaceFirst(lowerKw, "a")).get.length == 1
  }

  private def regexMatcher(regex: Regex, mkTok: (str: String, pos: Position) => GradCCToken): Matcher = {
    (str: String, pos: Position) => regex.findPrefixOf(str).map(mkTok(_, pos))
  }

  private val matchers: Seq[Matcher] =
    List(commentMatcher) ++
      Operator.values.sortBy(-_.str.length).map(operatorMatcher) ++
      Keyword.values.sortBy(-_.str.length).map(keywordMatcher) ++
      List(
        regexMatcher(lowerWordRegex, LowerWordToken(_, _)),
        regexMatcher(upperWordRegex, UpperWordToken(_, _)),
        regexMatcher("(\\s)+".r, SpaceToken(_, _)),
        regexMatcher("(\\S)+".r, ErrorToken(_, _))
      )

  {
    // Internal check
    var idx = 0
    for (matcher <- matchers) {
      if (matcher.accept("", null).isDefined) {
        throw IllegalStateException(s"matcher at index $idx accepts the empty string")
      }
      idx += 1
    }
  }

  private val lazyMatchers = LazyList.from(matchers)

  override val acceptsFaultyInput: Boolean = false

  override protected def runImpl(in: (Code, Filename), reporter: Reporter): Seq[GradCCToken] = {
    val (code, filename) = in
    val tokens = List.newBuilder[GradCCToken]
    var lineNumber = 1
    val lines = code.lines().toArray(new Array[String](_))
    val lastLineIdx = lines.size
    for (line <- lines) {
      var rem = line
      var columnNumber = 1
      var lastLine = ""
      while (rem.nonEmpty) {
        val position = Position(filename, lineNumber, columnNumber, line)
        val tok =
          lazyMatchers.map(_.accept(rem, position))
            .filter(_.isDefined)
            .map(_.get)
            .headOption
            .getOrElse(throw AssertionError(s"No matcher matched $rem"))
        tokens.addOne(tok)
        if (tok.isInstanceOf[ErrorToken]) {
          reporter.error(s"unrecognized character sequence: ${tok.str}", position)
        }
        val len = tok.str.length
        columnNumber += len
        rem = rem.substring(len)
        lastLine = line
      }
      if (lineNumber == lastLineIdx) {
        tokens.addOne(EndOfFileToken(Position(filename, lineNumber, columnNumber, lastLine)))
      }
      lineNumber += 1
    }
    tokens.result()
  }

  private trait Matcher {
    def accept(str: String, pos: Position): Option[GradCCToken]
  }

}

object ScannerPhase {
  val lowerWordRegex: Regex = "(?:[a-z]|_)(?:[A-Z]|[a-z]|[0-9]|_)*".r
  val upperWordRegex: Regex = "[A-Z](?:[A-Z]|[a-z]|[0-9]|_)*".r
}
