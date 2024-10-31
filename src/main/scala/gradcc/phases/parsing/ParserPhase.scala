package gradcc.phases.parsing

import gradcc.*
import gradcc.asts.AmbiguouslyNamedTerms
import gradcc.asts.AmbiguouslyNamedTerms.*
import gradcc.lang.Keyword.*
import gradcc.lang.Operator.*
import gradcc.lang.{Keyword, NamedField, Operator, RegionField}
import gradcc.phases.SimplePhase
import gradcc.reporting.{Position, Reporter}

import scala.annotation.tailrec

final class ParserPhase extends SimplePhase[Seq[GradCCToken], TermTree]("Parser") {

  private type Tokens = List[GradCCToken]

  override val acceptsFaultyInput: Boolean = false

  override protected def runImpl(in: Seq[GradCCToken], reporter: Reporter): TermTree = {

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

    def expectOperator[T](op: Operator, tokens: Tokens, msg: => String)(result: => T): (T, Tokens) = tokens match {
      case OperatorToken(`op`, _) :: rem => (result, rem)
      case _ => reporter.fatal(msg, tokens.headPos)
    }

    def parseProperPath(tokens: Tokens, errorMsg: => String): (ProperPathTree, Tokens) = tokens match {
      case LowerWordToken(rootId, pos) :: rem => parsePathFollow(rem, IdentifierTree(rootId, pos))
      case _ => reporter.fatal(errorMsg, tokens.headPos)
    }

    def parseStablePath(tokens: Tokens, errorMsg: => String): (StablePathTree, Tokens) = tokens match {
      case OperatorToken(Brand, brandPos) :: rem1 =>
        val (properPath, rem2) = parseProperPath(rem1, "expected a path after brand symbol")
        (BrandedPathTree(properPath, brandPos), rem2)
      case rem => parseProperPath(rem, errorMsg)
    }

    @tailrec
    def parsePathFollow(tokens: Tokens, acc: ProperPathTree): (ProperPathTree, Tokens) = tokens match {
      case OperatorToken(Dot, dotPos) :: LowerWordToken(fld, fldPos) :: rem =>
        parsePathFollow(rem, SelectTree(acc, NamedFieldTree(fld, fldPos), dotPos))
      case OperatorToken(Dot, dotPos) :: KeywordToken(RegKw, fldPos) :: rem =>
        parsePathFollow(rem, SelectTree(acc, RegFieldTree(fldPos), dotPos))
      case _ => (acc, tokens)
    }

    def parseAssignFollow(tokens: Tokens, lhs: StablePathTree): (AssignTree, Tokens) = tokens match {
      case OperatorToken(ColumnEqual, pos) :: rem1 =>
        val (rhs, rem2) = parseStablePath(rem1, s"expected a path after $ColumnEqual")
        (AssignTree(lhs, rhs, pos), rem2)
      case _ => reporter.fatal("expected ':='", tokens.headPos)
    }

    def parseDotRefFollow(tokens: Tokens, lhs: StablePathTree): (RefTree, Tokens) = tokens match {
      case OperatorToken(Dot, dotPos) :: KeywordToken(RefKw, _) :: rem1 =>
        val (rhs, rem2) = parseStablePath(rem1, s"expected a path after $RefKw")
        (RefTree(lhs, rhs, dotPos), rem2)
      case _ => reporter.fatal(s"expected '.$RefKw'", tokens.headPos)
    }

    def parseValueOpt(tokens: Tokens): Option[(ValueTree, Tokens)] = tokens match {
      case KeywordToken(BoxKw, pos) :: rem1 => Some {
        val (p, rem2) = parseStablePath(rem1, s"expected a path after $BoxKw")
        (BoxTree(p, pos), rem2)
      }
      case KeywordToken(FnKw, fnPos) :: rem1 => Some {
        val (abs, rem2) = parseFnFollow(rem1, true, parseTerm)
        (abs.asInstanceOf[AbsTree], rem2)
      }
      case OperatorToken(OpenBrace, openBracePos) :: rem1 =>
        val (fields, rem2) = parseCommaSeparatedFieldValuePairs(rem1)
        rem2 match {
          case OperatorToken(CloseBrace, _) :: rem3 =>
            Some((RecordLiteralTree(fields, openBracePos), rem3))
          case _ => None
        }
      case OperatorToken(OpenParenth, pos) :: OperatorToken(CloseParenth, _) :: rem =>
        Some((UnitLiteralTree(pos), rem))
      case _ => None
    }

    def parseFnFollow(tokens: Tokens, isFirst: Boolean, bodyParser: Tokens => (TermTree, Tokens)): (TermTree, Tokens) = {
      parseVarColonTypeBetweenParenthesesOpt(tokens) match {
        case (Some((varId, varType)), rem1) =>
          val (body, rem2) = parseFnFollow(rem1, false, bodyParser)
          (AbsTree(varId, varType, body, tokens.head.pos), rem2)
        case _ if isFirst => reporter.fatal("malformed function: could not parse arguments list", tokens.headPos)
        case (None, rem1) => bodyParser(rem1)
      }
    }

    def parseCommaSeparatedFieldValuePairs(tokens: Tokens): (List[(FieldTree, StablePathTree)], Tokens) = tokens match {
      case LowerWordToken(fld, fldPos) :: OperatorToken(Equal, _) :: rem1 =>
        val r2@(p, rem2) = parseStablePath(rem1, s"expected a path after $Equal")
        val head = (NamedFieldTree(fld, fldPos), p)
        rem2 match {
          case OperatorToken(Comma, _) :: rem3 =>
            val (tail, rem4) = parseCommaSeparatedFieldValuePairs(rem3)
            (head :: tail, rem4)
          case _ => (List(head), rem2)
        }
      case _ => (Nil, tokens)
    }

    def parseCommaSeparatedFieldTypePairs(tokens: Tokens): (List[(NamedFieldTree, TypeTree)], Tokens) = tokens match {
      case LowerWordToken(fld, fldPos) :: OperatorToken(Colon, _) :: rem1 =>
        val (tpe, rem2) = parseType(rem1)
        val head = (NamedFieldTree(fld, fldPos), tpe)
        rem2 match {
          case OperatorToken(Comma, _) :: rem3 =>
            val (tail, rem4) = parseCommaSeparatedFieldTypePairs(rem3)
            (head :: tail, rem4)
          case _ => (List(head), rem2)
        }
      case _ => (Nil, tokens)
    }

    def parseCommaSeparatedProperPaths(tokens: Tokens): (List[ProperPathTree], Tokens) = tokens match {
      case LowerWordToken(_, _) :: _ =>
        val (head, rem1) = parseProperPath(tokens, assert(false))
        rem1 match {
          case OperatorToken(Comma, _) :: rem2 =>
            val (tail, rem3) = parseCommaSeparatedProperPaths(rem2)
            (head :: tail, rem3)
          case _ => (List(head), rem1)
        }
      case _ => (Nil, tokens)
    }

    def parseVarColonTypeBetweenParenthesesOpt(tokens: Tokens): (Option[(IdentifierTree, TypeTree)], Tokens) =
      tokens match {
        case OperatorToken(OpenParenth, _) :: LowerWordToken(varId, varPos) :: OperatorToken(Colon, _) :: rem1 =>
          val (tpe, rem2) = parseType(rem1)
          rem2 match {
            case OperatorToken(CloseParenth, _) :: rem3 =>
              (Some(IdentifierTree(varId, varPos), tpe), rem3)
            case _ =>
              reporter.fatal("unclosed parenthesis after argument", rem2.headPos)
          }
        case _ => (None, tokens)
      }

    def parseLetFollow(id: String, tpe: Option[TypeTree], tokens: Tokens, letPos: Position, idPos: Position): (LetTree, Tokens) = {
      val (value, rem) = parseTerm(tokens)
      rem match {
        case KeywordToken(InKw, _) :: rem4 =>
          val (body, rem5) = parseTerm(rem4)
          (LetTree(IdentifierTree(id, idPos), value, tpe, body, letPos), rem5)
        case _ =>
          reporter.fatal("malformed let, expected 'let <var> = <value> in <body>'", rem.headPos)
      }
    }

    def parseTerm(tokens: Tokens): (TermTree, Tokens) = tokens match {

      // cases starting with a path
      case _ if startsWithLowerWord(tokens) =>
        val res1@(p1, rem1) = parseStablePath(tokens, assert(false))
        if (startsWithLowerWord(rem1)) {
          val (p2, rem2) = parseStablePath(rem1, assert(false))
          (AppTree(p1, p2, p1.position), rem2)
        } else if (startsWithOperator(ColumnEqual, rem1)) {
          parseAssignFollow(rem1, p1)
        } else if (startsWithOperator(Dot, rem1)) {
          parseDotRefFollow(rem1, p1)
        } else res1

      // let-binding
      case KeywordToken(LetKw, letPos) :: rem1 => {
        rem1 match {
          case LowerWordToken(id, idPos) :: OperatorToken(Equal, _) :: rem2 =>
            parseLetFollow(id, None, rem2, letPos, idPos)
          case LowerWordToken(id, idPos) :: OperatorToken(Colon, _) :: rem2 =>
            val (tpe, rem3) = parseType(rem2)
            rem3 match {
              case OperatorToken(Equal, _) :: rem4 =>
                parseLetFollow(id, Some(tpe), rem4, letPos, idPos)
              case _ =>
                reporter.fatal("malformed let, expected '=' after variable", rem3.headPos)
            }
          case _ =>
            reporter.fatal("malformed let, expected 'let <var> = <value> in <body>'", rem1.headPos)
        }
      }

      // region
      case KeywordToken(RegionKw, pos) :: rem => (RegionTree(pos), rem)

      // dereference
      case OperatorToken(Bang, pos) :: rem1 =>
        val (p, rem2) = parseStablePath(rem1, s"expected a path after $Bang")
        (DerefTree(p, pos), rem2)

      // module
      case KeywordToken(ModKw, modPos) :: rem1 => {
        def reportMalformedModule(remTokens: Tokens) = reporter.fatal("malformed module", remTokens.headPos)

        rem1 match {
          case OperatorToken(OpenParenth, _) :: rem2 =>
            val (region, rem3) = parseStablePath(rem2, "expected a path as module region")
            rem3 match {
              case OperatorToken(CloseParenth, _) :: OperatorToken(OpenBrace, _) :: rem4 =>
                val (fields, rem5) = parseCommaSeparatedFieldValuePairs(rem4)
                expectOperator(CloseBrace, rem5, reportMalformedModule(rem5)) {
                  ModuleTree(region, fields, modPos)
                }
              case _ => reportMalformedModule(rem3)
            }
          case _ => reportMalformedModule(rem1)
        }
      }

      // empty record
      case OperatorToken(OpenBrace, pos) :: OperatorToken(CloseBrace, _) :: rem => {
        if (startsWithKeyword(UnboxKw, rem)) {
          reporter.fatal("capture set cannot be empty for unbox form", pos)
        }
        (RecordLiteralTree(Seq.empty, pos), rem)
      }

      // non-empty record
      case OperatorToken(OpenBrace, openBracePos) :: (rem1@(LowerWordToken(_, _) :: OperatorToken(Equal, _) :: _)) => {
        val (fields, rem2) = parseCommaSeparatedFieldValuePairs(rem1)
        rem2 match {
          case OperatorToken(CloseBrace, _) :: rem3 =>
            (RecordLiteralTree(fields, openBracePos), rem3)
          case _ => reporter.fatal(s"unclosed record (missing '$CloseBrace", rem2.headPos)
        }
      }

      // unbox
      case OperatorToken(OpenBrace, openBracePos) :: (rem1@(LowerWordToken(_, _) :: _)) => {
        val (capturedPaths, rem2) = parseCommaSeparatedProperPaths(rem1)
        val captureSetTree = NonRootCaptureSetTree(capturedPaths, openBracePos)
        rem2 match {
          case OperatorToken(CloseBrace, _) :: KeywordToken(UnboxKw, unboxPos) :: rem3 =>
            val (p, rem4) = parseProperPath(rem3, s"expected a path after $UnboxKw")
            (UnboxTree(captureSetTree, p, unboxPos), rem4)
          case _ => reporter.fatal("malformed unbox term", rem2.headPos)
        }
      }

      case OperatorToken(OpenBrace, _) :: rem =>
        reporter.fatal("unrecognized term: neither a record, nor an unbox term, could be constructed", rem.headPos)

      case OperatorToken(OpenParenth, pos) :: OperatorToken(CloseParenth, _) :: rem =>
        (UnitLiteralTree(pos), rem)

      case OperatorToken(OpenParenth, _) :: rem1 =>
        val (term, rem2) = parseTerm(rem1)
        expectOperator(CloseParenth, rem2, "missing closing parenthesis")(term)

      case KeywordToken(BoxKw, pos) :: rem1 =>
        val (p, rem2) = parseStablePath(rem1, s"expected a path after $BoxKw")
        (BoxTree(p, pos), rem2)

      case KeywordToken(FnKw, fnPos) :: rem1 =>
        val (abs, rem2) = parseFnFollow(rem1, true, parseTerm)
        (abs.asInstanceOf[AbsTree], rem2)

      case KeywordToken(EnclKw, enclPos) :: OperatorToken(OpenSquareBracket, _) :: OperatorToken(OpenBrace, openBrPos) :: rem1 =>
        val (permissionsVars, rem2) = parseCommaSeparatedProperPaths(rem1)
        rem2 match {
          case OperatorToken(CloseBrace, _) :: OperatorToken(CloseSquareBracket, _)
            :: OperatorToken(OpenSquareBracket, _) :: rem3 => {
            val (tpe, rem4) = parseType(rem3)
            rem4 match {
              case OperatorToken(CloseSquareBracket, _) :: rem5 =>
                val (body, rem6) = parseTerm(rem5)
                (EnclosureTree(NonRootCaptureSetTree(permissionsVars, openBrPos), tpe, body, enclPos), rem6)
              case _ =>
                reporter.fatal("expected ']' after type", rem4.headPos)
            }
          }
          case _ =>
            reporter.fatal("expected end of enclosure term", rem2.headPos)
        }

      case KeywordToken(ObscurKw, obscurPos) :: rem1 =>
        val (obscuredP, rem2) = parseStablePath(rem1, s"expected path after $ObscurKw")
        rem2 match {
          case KeywordToken(AsKw, _) :: LowerWordToken(varId, varPos) :: KeywordToken(InKw, _) :: rem3 =>
            val (body, rem4) = parseTerm(rem3)
            (ObscurTree(obscuredP, IdentifierTree(varId, varPos), body, obscurPos), rem4)
          case _ => reporter.fatal("malformed obscur form: expected 'obscur <path> as <id> in <term>'", rem2.headPos)
        }

      case _ => reporter.fatal("expected a term", tokens.headPos)
    }

    def parseType(tokens: List[GradCCToken]): (TypeTree, Tokens) = {
      val (t, rem) = parseShapeOrType(tokens)
      t match {
        case typeTree: TypeTree => (typeTree, rem)
        case shapeTree: ShapeTree => (TypeTree(shapeTree, None, shapeTree.position), rem)
      }
    }

    def parseShapeOrType(tokens: Tokens): (ShapeTree | TypeTree, Tokens) = {
      parseVarColonTypeBetweenParenthesesOpt(tokens) match {
        case (Some((varId, varType)), OperatorToken(Arrow, _) :: OperatorToken(OpenBrace, openBrPos) :: rem1) =>
          val (paths, rem2) = parseCommaSeparatedProperPaths(rem1)
          rem2 match {
            case OperatorToken(CloseBrace, _) :: rem3 =>
              val (resType, rem4) = parseType(rem3)
              (TypeTree(
                AbsShapeTree(varId, varType, resType, tokens.head.pos),
                Some(NonRootCaptureSetTree(paths, openBrPos)),
                tokens.head.pos
              ), rem4)
            case _ => reporter.fatal("unclosed capture set", rem2.headPos)
          }
        case (Some((varId, varType)), OperatorToken(Arrow, _) :: rem1) =>
          val (resType, rem2) = parseType(rem1)
          maybeWithCaptureSet(AbsShapeTree(varId, varType, resType, tokens.head.pos), rem2)
        case _ => tokens match {
          case KeywordToken(TopKw, pos) :: rem =>
            maybeWithCaptureSet(TopShapeTree(pos), rem)
          case KeywordToken(BoxKw, pos) :: rem1 =>
            val (boxedType, rem2) = parseType(rem1)
            forbidCaptureSet(BoxShapeTree(boxedType, pos), rem2, "a boxed type cannot have a capture set")
          case KeywordToken(UnitKw, pos) :: rem =>
            forbidCaptureSet(UnitShapeTree(pos), rem, s"$UnitKw cannot have a capture set")
          case KeywordToken(RefKw, pos) :: rem1 =>
            val (shape, rem2) = {
              val (t, rem) = parseShapeOrType(rem1)
              (forbidCapture(t, "only shape types can be turned into references, " +
                s"but a non-empty capture set was found"), rem)
            }
            maybeWithCaptureSet(RefShapeTree(shape, pos), rem2)
          case KeywordToken(RegKw, pos) :: rem =>
            maybeWithCaptureSet(RegShapeTree(pos), rem)
          case KeywordToken(SelfKw, selfPos) :: rem1 =>
            rem1 match {
              case LowerWordToken(selfId, idPos) :: KeywordToken(InKw, _) :: OperatorToken(OpenBrace, _) :: rem2 =>
                val (fieldTypes, rem3) = parseCommaSeparatedFieldTypePairs(rem2)
                val (rtt, rem4) = expectOperator(CloseBrace, rem3, "unclosed fields list in record shape") {
                  RecordShapeTree(Some(IdentifierTree(selfId, idPos)), fieldTypes, selfPos)
                }
                maybeWithCaptureSet(rtt, rem4)
              case _ => reporter.fatal("malformed self-referencing record type", rem1.headPos)
            }
          case OperatorToken(OpenBrace, openBrPos) :: rem1 =>
            val (fieldTypes, rem2) = parseCommaSeparatedFieldTypePairs(rem1)
            val (rtt, rem4) = expectOperator(CloseBrace, rem2, "unclosed fields list in record shape") {
              RecordShapeTree(None, fieldTypes, openBrPos)
            }
            maybeWithCaptureSet(rtt, rem4)
          case tokens => reporter.fatal("expected a type shape", tokens.headPos)
        }
      }
    }

    def maybeWithCaptureSet(shape: ShapeTree, tokens: Tokens): (ShapeTree | TypeTree, Tokens) = tokens match {
      case OperatorToken(Hat, hatPos) :: OperatorToken(OpenBrace, _) :: rem1 =>
        val (paths, rem2) = parseCommaSeparatedProperPaths(rem1)
        expectOperator(CloseBrace, rem2, "unclosed capture set") {
          TypeTree(shape, Some(NonRootCaptureSetTree(paths, hatPos)), shape.position)
        }
      case OperatorToken(Hat, hatPos) :: rem =>
        (TypeTree(shape, Some(RootCaptureSetTree(hatPos)), shape.position), rem)
      case _ => (shape, tokens)
    }

    def forbidCaptureSet(shape: ShapeTree, tokens: Tokens, msg: => String): (ShapeTree, Tokens) = {
      val (t, rem) = maybeWithCaptureSet(shape, tokens)
      (forbidCapture(t, msg), rem)
    }

    def forbidCapture(t: (ShapeTree | TypeTree), msg: => String): ShapeTree = t match {
      case typeTree: TypeTree =>
        if (typeTree.captureSet.nonEmpty) {
          reporter.error(msg, typeTree.captureSet.head.position)
        }
        typeTree.shape
      case shapeTree: ShapeTree => shapeTree
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
