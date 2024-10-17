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

    def parsePath(tokens: Tokens, errorMsg: => String): (Path, Tokens) = tokens match {
      case LowerWordToken(rootId, pos) :: rem => parsePathFollow(rem, Identifier(rootId, pos))
      case _ => reporter.fatal(errorMsg, tokens.headPos)
    }

    @tailrec
    def parsePathFollow(tokens: Tokens, acc: Path): (Path, Tokens) = tokens match {
      case OperatorToken(Dot, pos) :: LowerWordToken(fld, _) :: rem =>
        parsePathFollow(rem, Select(acc, fld, pos))
      case _ => (acc, tokens)
    }

    def parseAssignFollow(tokens: Tokens, lhs: Path): (Assign, Tokens) = tokens match {
      case OperatorToken(ColumnEqual, pos) :: rem1 =>
        val (rhs, rem2) = parsePath(rem1, s"expected a path after $ColumnEqual")
        (Assign(lhs, rhs, pos), rem2)
      case _ => reporter.fatal("expected ':='", tokens.headPos)
    }

    def parseDotRefFollow(tokens: Tokens, lhs: Path): (Ref, Tokens) = tokens match {
      case OperatorToken(Dot, dotPos) :: KeywordToken(RefLKw, _) :: rem1 =>
        val (rhs, rem2) = parsePath(rem1, s"expected a path after $RefLKw")
        (Ref(lhs, rhs, dotPos), rem2)
      case _ => reporter.fatal(s"expected '.$RefLKw'", tokens.headPos)
    }

    def parseValueOpt(tokens: Tokens): Option[(Value, Tokens)] = tokens match {
      case KeywordToken(BoxKw, pos) :: rem1 => Some {
        val (p, rem2) = parsePath(rem1, s"expected a path after $BoxKw")
        (Box(p, pos), rem2)
      }
      case KeywordToken(FnKw, fnPos) :: rem1 => Some {
        val (abs, rem2) = parseFnFollow(rem1, true, parseTerm)
        (abs.asInstanceOf[Abs], rem2)
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

    def parseFnFollow(tokens: Tokens, isFirst: Boolean, bodyParser: Tokens => (Term, Tokens)): (Term, Tokens) = {
      parseVarColonTypeBetweenParenthesesOpt(tokens) match {
        case (Some((varId, varType)), rem1) =>
          val (body, rem2) = parseFnFollow(rem1, false, bodyParser)
          (Abs(varId, varType, body, tokens.head.pos), rem2)
        case _ if isFirst => reporter.fatal("malformed function: could not parse arguments list", tokens.headPos)
        case (None, rem1) => bodyParser(rem1)
      }
    }

    def parseCommaSeparatedFieldValuePairs(tokens: Tokens): (List[(FieldTree, Path)], Tokens) = tokens match {
      case LowerWordToken(fld, fldPos) :: OperatorToken(Equal, _) :: rem1 =>
        val r2@(p, rem2) = parsePath(rem1, s"expected a path after $Equal")
        val head = (NamedField(fld, fldPos), p)
        rem2 match {
          case OperatorToken(Comma, _) :: rem3 =>
            val (tail, rem4) = parseCommaSeparatedFieldValuePairs(rem3)
            (head :: tail, rem4)
          case _ => (List(head), rem2)
        }
      case _ => (Nil, tokens)
    }

    def parseCommaSeparatedFieldTypePairs(tokens: Tokens): (List[(NamedField, TypeTree)], Tokens) = tokens match {
      case LowerWordToken(fld, fldPos) :: OperatorToken(Colon, _) :: rem1 =>
        val (tpe, rem2) = parseType(rem1)
        val head = (NamedField(fld, fldPos), tpe)
        rem2 match {
          case OperatorToken(Comma, _) :: rem3 =>
            val (tail, rem4) = parseCommaSeparatedFieldTypePairs(rem3)
            (head :: tail, rem4)
          case _ => (List(head), rem2)
        }
      case _ => (Nil, tokens)
    }

    def parseCommaSeparatedPaths(tokens: Tokens): (List[Path], Tokens) = tokens match {
      case LowerWordToken(_, _) :: _ =>
        val (head, rem1) = parsePath(tokens, assert(false))
        rem1 match {
          case OperatorToken(Comma, _) :: rem2 =>
            val (tail, rem3) = parseCommaSeparatedPaths(rem2)
            (head :: tail, rem3)
          case _ => (List(head), rem1)
        }
      case _ => (Nil, tokens)
    }

    def parseVarColonTypeBetweenParenthesesOpt(tokens: Tokens): (Option[(Identifier, TypeTree)], Tokens) =
      tokens match {
        case OperatorToken(OpenParenth, _) :: LowerWordToken(varId, varPos) :: OperatorToken(Colon, _) :: rem1 =>
          val (tpe, rem2) = parseType(rem1)
          rem2 match {
            case OperatorToken(CloseParenth, _) :: rem3 =>
              (Some(Identifier(varId, varPos), tpe), rem3)
            case _ => (None, rem2)
          }
        case _ => (None, tokens)
      }

    def parseTerm(tokens: Tokens): (Term, Tokens) = tokens match {

      // cases starting with a path
      case _ if startsWithLowerWord(tokens) =>
        val res1@(p1, rem1) = parsePath(tokens, assert(false))
        if (startsWithLowerWord(rem1)) {
          val (p2, rem2) = parsePath(rem1, assert(false))
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
        val (p, rem2) = parsePath(rem1, s"expected a path after $Bang")
        (Deref(p, pos), rem2)

      // module
      case KeywordToken(ModKw, modPos) :: rem1 =>
        def reportMalformedModule(remTokens: Tokens) = reporter.fatal("malformed module", remTokens.headPos)

        rem1 match {
          case OperatorToken(OpenParenth, _) :: rem2 =>
            val (region, rem3) = parsePath(rem2, "expected a path as module region")
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
                  val (p, rem4) = parsePath(rem3, s"expected a path after $UnboxKw")
                  (Unbox(captureSetTree, p, unboxPos), rem4)
                case _ => reporter.fatal("malformed unbox term", rem2.headPos)
              }
            case _ => reporter.fatal("expected a term", tokens.headPos)
        }

    }

    def parseType(tokens: Tokens): (TypeTree, Tokens) = {
      val (shape, rem1) = parseShape(tokens)
      rem1 match {
        case OperatorToken(Hat, hatPos) :: OperatorToken(OpenBrace, _) :: rem2 =>
          val (paths, rem3) = parseCommaSeparatedPaths(rem2)
          rem3 match {
            case OperatorToken(CloseBrace, _) :: rem4 =>
              (TypeTree(shape, Some(NonRootCaptureSet(paths, hatPos)), shape.position), rem4)
            case _ =>
              reporter.fatal("missing closing brace after capture set", rem3.headPos)
          }
        case OperatorToken(Hat, hatPos) :: rem2 =>
          (TypeTree(shape, Some(RootCaptureSet(hatPos)), shape.position), rem2)
        case _ =>
          (TypeTree(shape, None, shape.position), rem1)
      }
    }

    def parseShape(tokens: Tokens): (TypeShapeTree, Tokens) = {
      parseVarColonTypeBetweenParenthesesOpt(tokens) match {
        case (Some(varIdent, varType), rem1) =>
          rem1 match {
            case OperatorToken(Arrow, _) :: rem2 =>
              val (resType, rem3) = parseType(rem2)
              (AbsTypeTree(varIdent, varType, resType, tokens.head.pos), rem3)
            case _ => reporter.fatal(s"expected $Arrow", rem1.headPos)
          }
        case _ => tokens match {
          case KeywordToken(TopKw, pos) :: rem =>
            (TopTypeTree(pos), rem)
          case KeywordToken(BoxKw, pos) :: rem1 =>
            val (boxedType, rem2) = parseType(rem1)
            (BoxTypeTree(boxedType, pos), rem2)
          case KeywordToken(UnitKw, pos) :: rem =>
            (UnitTypeTree(pos), rem)
          case KeywordToken(RefUKw, pos) :: rem1 =>
            val (shape, rem2) = parseShape(rem1)
            (RefTypeTree(shape, pos), rem2)
          case KeywordToken(RegUKw, pos) :: rem =>
            (RegTypeTree(pos), rem)
          case KeywordToken(SelfKw, selfPos) :: rem1 =>
            rem1 match {
              case LowerWordToken(selfId, idPos) :: KeywordToken(InKw, _) :: OperatorToken(OpenBrace, _) :: rem2 =>
                val (fieldTypes, rem3) = parseCommaSeparatedFieldTypePairs(rem2)
                rem3 match {
                  case OperatorToken(CloseBrace, _) :: rem4 =>
                    (RecordTypeTree(Some(Identifier(selfId, idPos)), fieldTypes, selfPos), rem4)
                  case _ => reporter.fatal("unclosed fields list", rem3.headPos)
                }
              case _ => reporter.fatal("malformed self-referencing record type", rem1.headPos)
            }
          case OperatorToken(OpenBrace, openBrPos) :: rem1 =>
            val (fieldTypes, rem2) = parseCommaSeparatedFieldTypePairs(rem1)
            rem2 match {
              case OperatorToken(CloseBrace, _) :: rem2 =>
                (RecordTypeTree(None, fieldTypes, openBrPos), rem2)
              case _ => reporter.fatal("unclosed fields list", rem2.headPos)
            }
          case tokens => reporter.fatal("expected a type shape", tokens.headPos)
        }
      }
    }

    parseTerm(filterIsCoreToken(in)) match {
      case (term, Nil) =>
        reporter.warning("expected end of input", None)
        term
      case (term, List(EndOfFileToken(_))) =>
        term
      case (_, rem) =>
        reporter.fatal("unexpected tokens at the end of the input", rem.headPos)
    }
  }

  private def filterIsCoreToken(tokens: Seq[GradCCToken]): List[CoreGradCCToken] = {
    val b = List.newBuilder[CoreGradCCToken]
    tokens foreach {
      case tok: CoreGradCCToken =>
        b.addOne(tok)
      case _ => ()
    }
    b.result()
  }

  extension (tokens: Tokens) private def headPos: Option[Position] =
    tokens.headOption.map(_.pos)

  case class ParsingException(msg: String) extends RuntimeException(msg)

}
