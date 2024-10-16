package gradcc.parsing

import gradcc.*
import gradcc.asts.AmbiguouslyNamedTerms
import gradcc.asts.AmbiguouslyNamedTerms.*
import gradcc.lang.Keyword.*
import gradcc.lang.Operator.*
import gradcc.lang.{Keyword, Operator}

import scala.annotation.tailrec

final class ParserPhase extends SimplePhase[Seq[GradCCToken], Term]("Parser") {

  private type Tokens = List[GradCCToken]

  override protected def runImpl(in: Seq[GradCCToken], reporter: Reporter): Term = {

    def startsWithLowerWord(tokens: Tokens): Boolean = tokens match {
      case LowerWordToken(_, _) :: _ => true
      case _ => false
    }

    def startsWithKeyword(kw: Keyword, tokens: Tokens): Boolean = tokens match {
      case KeywordToken(`kw`, _) :: _ => true
      case _ => false
    }

    def startsWithOperator(op: Operator, tokens: Tokens): Boolean = tokens match {
      case OperatorToken(`op`, _) :: _ => true
      case _ => false
    }

    def parsePath(tokens: Tokens): (Path, Tokens) = tokens match {
      case LowerWordToken(rootId, pos) :: rem => parsePathFollow(rem, Identifier(rootId, pos))
      case _ => reporter.fatal("expected a path", tokens.headPos)
    }

    @tailrec
    def parsePathFollow(tokens: Tokens, acc: Path): (Path, Tokens) = tokens match {
      case OperatorToken(Dot, pos) :: LowerWordToken(fld, _) :: rem =>
        parsePathFollow(rem, Select(acc, fld, pos))
      case _ => (acc, tokens)
    }

    def parseAssignFollow(tokens: Tokens, lhs: Path): (Assign, Tokens) = tokens match {
      case OperatorToken(ColumnEqual, pos) :: rem1 =>
        val (rhs, rem2) = parsePath(rem1)
        (Assign(lhs, rhs, pos), rem2)
      case _ => reporter.fatal("expected ':='", tokens.headPos)
    }

    def parseDotRefFollow(tokens: Tokens, lhs: Path): (Ref, Tokens) = tokens match {
      case OperatorToken(Dot, dotPos) :: KeywordToken(RefLKw, _) :: rem1 =>
        val (rhs, rem2) = parsePath(rem1)
        (Ref(lhs, rhs, dotPos), rem2)
      case _ => reporter.fatal(s"expected '.$RefLKw'", tokens.headPos)
    }

    def parseValueOpt(tokens: Tokens): Option[(Value, Tokens)] = tokens match {
      case OperatorToken(BoxKw, pos) :: rem1 => Some {
        val (p, rem2) = parsePath(rem1)
        (Box(p, pos), rem2)
      }
      case OperatorToken(FnKw, fnPos) :: rem1 => Some {

        def reportMalformedFunction(nextTokens: Tokens) =
          reporter.fatal("malformed function", nextTokens.headPos)

        rem1 match {
          case OperatorToken(OpenParenth, _) :: LowerWordToken(varId, varPos) :: OperatorToken(Colon, _) :: rem2 =>
            val (tpe, rem3) = parseType(rem2)
            rem3 match {
              case OperatorToken(CloseParenth, _) :: rem4 =>
                val (body, rem5) = parseTerm(rem4)
                (Abs(Identifier(varId, varPos), tpe, body, fnPos), rem5)
              case _ => reportMalformedFunction(rem3)
            }
          case _ => reportMalformedFunction(rem1)
        }
      }
      case OperatorToken(OpenBrace, openBracePos) :: rem1 => Some {
        val (fields, rem2) = parseCommaSeparatedFieldValuePairs(rem1)
        rem2 match {
          case OperatorToken(CloseBrace, _) :: rem3 =>
            (RecordLiteral(fields, openBracePos), rem3)
          case _ => reporter.fatal("expected '}'", rem2.headPos)
        }
      }
      case OperatorToken(OpenParenth, pos) :: OperatorToken(CloseParenth, _) :: rem =>
        Some((UnitLiteral(pos), rem))
      case _ => None
    }

    def parseCommaSeparatedFieldValuePairs(tokens: Tokens): (List[(Field, Path)], Tokens) = {
      tokens match {
        case LowerWordToken(fld, fldPos) :: OperatorToken(Equal, _) :: rem1 =>
          val r2@(p, rem2) = parsePath(rem1)
          val head = (NamedField(fld, fldPos), p)
          rem2 match {
            case OperatorToken(Comma, _) :: rem3 =>
              val (tail, rem4) = parseCommaSeparatedFieldValuePairs(rem3)
              (head :: tail, rem4)
            case _ => (List(head), rem2)
          }
        case _ => (Nil, tokens)
      }
    }

    def parseCommaSeparatedPaths(tokens: Tokens): (List[Path], Tokens) = tokens match {
      case LowerWordToken(_, _) :: _ =>
        val (head, rem1) = parsePath(tokens)
        rem1 match {
          case OperatorToken(Comma, _) :: rem2 =>
            val (tail, rem3) = parseCommaSeparatedPaths(rem2)
            (head :: tail, rem3)
          case _ => (List(head), rem1)
        }
      case _ => (Nil, tokens)
    }

    def parseTerm(tokens: Tokens): (Term, Tokens) = tokens match {

      // cases starting with a path
      case _ if startsWithLowerWord(tokens) =>
        val res1@(p1, rem1) = parsePath(tokens)
        if (startsWithLowerWord(rem1)) {
          val (p2, rem2) = parsePath(rem1)
          (App(p1, p2, p1.position), rem2)
        } else if (startsWithOperator(ColumnEqual, rem1)) {
          parseAssignFollow(rem1, p1)
        } else if (startsWithOperator(Dot, rem1)) {
          parseDotRefFollow(rem1, p1)
        } else res1

      // let-binding
      case KeywordToken(LetKw, letPos) :: rem1 =>
        def reportMalformedLet(remTokens: Tokens) =
          reporter.fatal("malformed let, expected 'let <var> = <value> in <body>'", remTokens.headPos)

        rem1 match {
          case LowerWordToken(id, idPos) :: OperatorToken(Equal, _) :: rem2 =>
            val (value, rem3) = parseTerm(rem2)
            rem3 match {
              case KeywordToken(InKw, _) :: rem4 =>
                val (body, rem5) = parseTerm(rem4)
                (Let(Identifier(id, idPos), value, body, letPos), rem5)
              case _ => reportMalformedLet(rem3)
            }
          case _ => reportMalformedLet(rem1)
        }

      // region
      case KeywordToken(RegionKw, pos) :: rem => (Region(pos), rem)

      // dereference
      case OperatorToken(Bang, pos) :: rem1 =>
        val (p, rem2) = parsePath(rem1)
        (Deref(p, pos), rem2)

      // module
      case KeywordToken(ModKw, modPos) :: rem1 =>
        def reportMalformedModule(remTokens: Tokens) = reporter.fatal("malformed module", remTokens.headPos)

        rem1 match {
          case OperatorToken(OpenParenth, _) :: rem2 =>
            val (region, rem3) = parsePath(rem2)
            rem3 match {
              case OperatorToken(CloseParenth, _) :: OperatorToken(OpenBrace, _) :: rem4 =>
                val (fields, rem5) = parseCommaSeparatedFieldValuePairs(rem4)
                rem5 match {
                  case OperatorToken(CloseBrace, _) :: rem6 =>
                    (Module(region, fields, modPos), rem6)
                  case _ =>
                    reportMalformedModule(rem5)
                }
              case _ => reportMalformedModule(rem3)
            }
          case _ => reportMalformedModule(rem1)
        }

      // special case, ambiguous between record literal and empty capture set for unboxing
      case OperatorToken(OpenBrace, pos) :: OperatorToken(CloseBrace, _) :: rem =>
        if (startsWithKeyword(UnboxKw, rem)) {
          reporter.fatal("capture set cannot be empty for unbox form", pos)
        }
        (RecordLiteral(Seq.empty, pos), rem)

      // remaining cases: value or unboxing
      case tokens =>
        parseValueOpt(tokens).getOrElse {
          tokens match
            case OperatorToken(OpenBrace, openBracePos) :: rem1 =>
              val (capturedPaths, rem2) = parseCommaSeparatedPaths(rem1)
              val captureSetTree = NonRootCaptureSet(capturedPaths, openBracePos)
              rem2 match {
                case OperatorToken(CloseBrace, _) :: KeywordToken(UnboxKw, unboxPos) :: rem3 =>
                  val (p, rem4) = parsePath(rem3)
                  (Unbox(captureSetTree, p, unboxPos), rem4)
                case _ => reporter.fatal("malformed unbox term", rem2.headPos)
              }
            case _ => reporter.fatal("expected a term", tokens.headPos)
        }

    }

    def parseType(tokens: Tokens): (TypeTree, Tokens) = ???
    
    def parseShape(tokens: Tokens): (TypeShapeTree, Tokens) = tokens match {
      case KeywordToken(TopKw, pos) :: rem =>
        (TopTypeTree(pos), rem)
      // TODO
    }

    val (term, remTokens) = parseTerm(in.toList)
    if (remTokens.nonEmpty) {
      reporter.fatal("unexpected tokens at the end of the input", remTokens.head.pos)
    }
    term
  }

  extension (tokens: Tokens) private def headPos: Option[Position] =
    tokens.headOption.map(_.pos)

  case class ParsingException(msg: String) extends RuntimeException(msg)

}
