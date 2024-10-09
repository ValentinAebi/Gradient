package gradcc.parsing

import commons.{Reporter, SimplePhase}
import gradcc.asts.AmbiguouslyNamedTerms.*
import gradcc.lang.Keyword.*
import gradcc.lang.Operator.*
import gradcc.lang.{Keyword, Operator}
import gradcc.*

import scala.collection.immutable.ArraySeq
import scala.util.parsing.combinator.Parsers

final class ParserPhase extends SimplePhase[Seq[GradCCToken], Term]("Parser"), Parsers {

  override type Elem = GradCCToken

  // ----- Syntax primitives --------------------------

  private val lower = acceptMatch("[identifier]", {
    case tok: LowerWordToken => tok
  })

  private val upper = acceptMatch("[type identifier]", {
    case tok: UpperWordToken => tok
  })

  private def kw(kw: Keyword) = acceptMatch(s"[${kw.str}]", {
    case tok: KeywordToken if tok.keyword == kw => tok
  })

  private def op(op: Operator) = acceptMatch(s"[${op.str}]", {
    case tok: OperatorToken if tok.operator == op => tok
  })

  private lazy val eof = acceptMatch("[eof]", {
    case _: EndOfFileToken => ()
  })

  private val dot = op(Dot)
  private val comma = op(Comma)
  private val colon = op(Colon)
  private val equal = op(Equal)
  private val openParenth = op(OpenParenth)
  private val closeParenth = op(CloseParenth)
  private val openBrace = op(OpenBrace)
  private val closeBrace = op(CloseBrace)

  private type P[T] = Parser[T]

  // ----- Syntax -------------------------------------

  private lazy val identifier = lower.map {
    case LowerWordToken(str, pos) => Identifier(str, pos)
  }

  private lazy val cap: P[Cap] = kw(CapKw).map {
    case KeywordToken(kw, pos) => Cap(pos)
  }

  private lazy val variable: P[Variable] = identifier OR cap

  private lazy val namedField: P[NamedField] = lower.map {
    case LowerWordToken(str, pos) => NamedField(str, pos)
  }
  
  private lazy val reg: P[Reg] = kw(RegLKw).map(tok => Reg(tok.pos))
  
  private lazy val field: P[Field] = namedField OR reg

  private lazy val path: P[Path] = (identifier ~ rep(dot ~ lower)).map {
    case root ~ selects => {
      selects.foldLeft[Path](root) {
        case (r, dotTok ~ fldTok) => Select(r, fldTok.str, dotTok.pos)
      }
    }
  }

  private lazy val box: P[Box] = (kw(BoxKw) ~ path).map {
    case box ~ p => Box(p, box.pos)
  }

  private lazy val abs: P[Abs] = (kw(FnKw) ~ openParenth ~ rep1sep(identifier ~ colon ~ tpe, comma) ~ closeParenth ~ term).map {
    case fn ~ _ ~ ((param1Id ~ _ ~ param1Type) :: subsequentParams) ~ _ ~ body => {
      val abs1Body = subsequentParams.foldRight(body) {
        (param, accBody) => {
          val id ~ _ ~ tpe = param
          Abs(id, tpe, accBody, id.position)
        }
      }
      Abs(param1Id, param1Type, abs1Body, fn.pos)
    }
  }

  private lazy val recordLit: P[RecordLiteral] = (opt(kw(SelfKw) ~ identifier ~ kw(InKw)) ~ (openBrace ~ rep(field ~ equal ~ path) ~ closeBrace)).map {
    case optSelfRef ~ (openBr ~ fields ~ _) => RecordLiteral(fields.map {
      case fld ~ _ ~ p => (fld, p)
    }, optSelfRef.map {
      case _ ~ id ~ _ => id
    }, openBr.pos)
  }

  private lazy val unitLit: P[UnitLiteral] = (openParenth ~ closeParenth).map {
    case openB ~ _ => UnitLiteral(openB.pos)
  }

  private lazy val value: P[Value] = box OR abs OR recordLit OR unitLit

  private lazy val unbox: P[Unbox] = (kw(UnboxKw) ~ path ~ kw(Using) ~ explicitCaptureSet).map {
    case unbox ~ p ~ _ ~ capSet => Unbox(capSet, p, unbox.pos)
  }

  private lazy val let: P[Let] = (kw(LetKw) ~ identifier ~ equal ~ term ~ kw(InKw) ~ term).map {
    case let ~ id ~ _ ~ value ~ _ ~ body => Let(id, value, body, let.pos)
  }

  private lazy val region: P[Region] = kw(RegionKw).map(reg => Region(reg.pos))

  private lazy val deref: P[Deref] = (op(Bang) ~ path).map {
    case bang ~ p => Deref(p, bang.pos)
  }

  private lazy val modif: P[Modif] =
    (kw(ModKw) ~ openParenth ~ path ~ closeParenth ~ openBrace ~ rep1sep(field ~ equal ~ path, comma) ~ closeBrace).map {
      case mod ~ _ ~ capRegion ~ _ ~ _ ~ fields ~ _ =>
        Modif(capRegion, fields.map {
          case id ~ _ ~ p => (id, p)
        }, mod.pos)
    }

  private lazy val app: P[App] = rep1(path).map(paths =>
    paths.reduceLeft[Term]((accCaller, arg) => App(accCaller, arg, accCaller.position)).asInstanceOf[App]
  )

  private lazy val assignment: P[Assign] = (path ~ colon ~ equal ~ path).map {
    case lhs ~ _ ~ _ ~ rhs => Assign(lhs, rhs, lhs.position)
  }

  private lazy val ref: P[Ref] = (path ~ dot ~ ref ~ path).map {
    case lhs ~ _ ~ _ ~ rhs => Ref(lhs, rhs, lhs.position)
  }

  private lazy val term: P[Term] =
    path OR value OR app OR unbox OR let OR region OR deref OR assignment OR ref OR modif OR parenthesizedTerm

  private lazy val parenthesizedTerm: P[Term] = (openParenth ~ term ~ closeParenth).map {
    case _ ~ t ~ _ => t
  }

  private lazy val topType: P[TopTypeTree] = kw(TopKw).map(top => TopTypeTree(top.pos))

  private lazy val depType: P[AbsTypeTree] = (kw(DepKw) ~ openParenth ~ identifier ~ colon ~ tpe ~ closeParenth ~ tpe).map {
    case dep ~ _ ~ param ~ _ ~ paramType ~ _ ~ bodyType => AbsTypeTree(param, paramType, bodyType, dep.pos)
  }

  private lazy val boxType: P[BoxTypeTree] = (kw(BoxKw) ~ tpe).map {
    case box ~ boxed => BoxTypeTree(boxed, box.pos)
  }

  private lazy val unitType: P[UnitTypeTree] = kw(UnitKw).map(unit => UnitTypeTree(unit.pos))

  private lazy val refType: P[RefTypeTree] = (kw(RefUKw) ~ typeShape).map {
    case ref ~ referenced => RefTypeTree(referenced, ref.pos)
  }

  private lazy val regType: P[RegTypeTree] = kw(RegUKw).map(reg => RegTypeTree(reg.pos))

  private lazy val recordType: P[RecordTypeTree] =
    (opt(kw(SelfKw) ~ identifier ~ kw(InKw)) ~ openBrace ~ rep(namedField ~ colon ~ tpe) ~ closeBrace).map {
      case idToksOpt ~ openB ~ fieldsToks ~ _ => {
        val idOpt = idToksOpt.map {
          case _ ~ id ~ _ => id
        }
        val fields = fieldsToks.map {
          case fldName ~ _ ~ fldType => (fldName, fldType)
        }
        val pos = idOpt.map(_.position).getOrElse(openB.pos)
        RecordTypeTree(fields, idOpt, pos)
      }
    }

  private lazy val typeShape: P[TypeShapeTree] = topType OR depType OR boxType OR unitType OR refType OR regType OR recordType

  private lazy val tpe: P[TypeTree] = (typeShape ~ opt(op(Hat) ~ opt(explicitCaptureSet))).map {
    case shape ~ Some(_ ~ Some(explicitCapSet)) =>
      TypeTree(shape, Some(explicitCapSet), shape.position)
    case shape ~ Some(hat ~ None) =>
      TypeTree(shape, Some(ImplicitCaptureSetTree(hat.pos)), shape.position)
    case shape ~ None =>
      TypeTree(shape, None, shape.position)
  }

  private lazy val explicitCaptureSet: P[ExplicitCaptureSetTree] = (openBrace ~ repsep(path, comma) ~ closeBrace).map {
    case openB ~ capPaths ~ _ => ExplicitCaptureSetTree(capPaths, openB.pos)
  }


  override protected def runImpl(in: Seq[GradCCToken], reporter: Reporter): Term = {
    val interestingTokens = filterIsKindedToken(in)
    val reader = ParsingReader.from(interestingTokens)
    (term ~ eof).apply(reader) match {
      case Success(result ~ (), remaining) => result
      case Failure(rawMsg, remaining) =>
        val expectedTokens = extractExpectedTokensList(rawMsg)
        val msg = s"unexpected token: '${remaining.first.str}', expected one of: " + expectedTokens.mkString("'", "', '", "'")
        reporter.fatal(msg, remaining.first.pos)
      case Error(msg, _) => throw RuntimeException(msg)
    }
  }

  private def filterIsKindedToken(tokens: Seq[GradCCToken]): ArraySeq[CoreGradCCToken] = {
    val b = ArraySeq.newBuilder[CoreGradCCToken]
    tokens foreach {
      case kindedGradCCToken: CoreGradCCToken =>
        b.addOne(kindedGradCCToken)
      case _ => ()
    }
    b.result()
  }

  // HACK (for better error messages)
  // This method is mostly copied from the `|||` combinator in Scala parser combinators library
  extension [T](p: Parser[T]) private infix def OR[U >: T](q0: => Parser[U]): Parser[U] = new Parser[U] {
    lazy val q: Parser[U] = q0 // lazy argument

    def apply(in: Input): ParseResult[U] = {
      val res1 = p(in)
      val res2 = q(in)

      ((res1, res2): @unchecked) match {
        case (s1@Success(_, next1), s2@Success(_, next2)) =>
          if (next2.pos < next1.pos || next2.pos == next1.pos) s1 else s2
        case (s1@Success(_, _), _) => s1
        case (_, s2@Success(_, _)) => s2
        case (e1@Error(_, _), _) => e1
        case (f1@Failure(msg1, next1), f2@Failure(msg2, next2)) if next1 == next2 =>
          Failure(combineExpectedTokens(msg1, msg2), next1)
        case (f1@Failure(_, next1), ns2@NoSuccess(_, next2)) =>
          if (next2.pos < next1.pos || next2.pos == next1.pos) f1 else ns2
      }
    }

    override def toString = "|||"
  }

  private def combineExpectedTokens(l: String, r: String): String = {
    val lTokens = extractExpectedTokensList(l)
    val rTokens = extractExpectedTokensList(r)
    (lTokens ++ rTokens).mkString("[", ",", "]")
  }

  private def extractExpectedTokensList(str: String): Array[String] =
    str.dropWhile(_ != '[').tail.takeWhile(_ != ']').split(',')

  case class ParsingException(msg: String) extends RuntimeException(msg)

}
