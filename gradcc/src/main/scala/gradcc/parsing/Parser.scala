package gradcc.parsing

import commons.{Reporter, SimplePhase}
import gradcc.*
import gradcc.lang.Keyword.*
import gradcc.lang.Operator.*
import gradcc.lang.{Keyword, Operator}

import scala.collection.immutable.ArraySeq
import scala.util.parsing.combinator.Parsers

class Parser extends SimplePhase[Seq[GradCCToken], Term]("Parser"), Parsers {

  override type Elem = GradCCToken

  // ----- Syntax primitives --------------------------

  private val lower = acceptMatch("identifier", {
    case tok: LowerWordToken => tok
  })

  private val upper = acceptMatch("type identifier", {
    case tok: UpperWordToken => tok
  })

  private def kw(kw: Keyword) = acceptMatch(s"'${kw.str}'", {
    case tok: KeywordToken if tok.keyword == kw => tok
  })

  private def op(op: Operator) = acceptMatch(s"'${op.str}'", {
    case tok: OperatorToken if tok.operator == op => tok
  })

  private lazy val eof = acceptMatch("end of file", {
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

  private lazy val variable: P[Variable] = identifier ||| cap

  private lazy val reg: P[Reg] = kw(RegKw).map(tok => Reg(tok.pos))
  private lazy val field: P[Field] = identifier ||| reg

  private lazy val path: P[Path] = rep1sep(identifier, dot).map {
    case List(h) => h
    case h :: t => CompoundPath(h, t, h.position)
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

  private lazy val recordLit: P[RecordLiteral] = (openBrace ~ rep(field ~ equal ~ path) ~ closeBrace).map {
    case openBr ~ fields ~ _ => RecordLiteral(fields.map {
      case fld ~ _ ~ p => (fld, p)
    }, openBr.pos)
  }

  private lazy val unitLit: P[UnitLiteral] = (openParenth ~ closeParenth).map {
    case openB ~ _ => UnitLiteral(openB.pos)
  }

  private lazy val value: P[Value] = box ||| abs ||| recordLit ||| unitLit

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
    path ||| value ||| app ||| unbox ||| let ||| region ||| deref ||| assignment ||| ref ||| modif ||| parenthesizedTerm

  private lazy val parenthesizedTerm: P[Term] = (openParenth ~ term ~ closeParenth).map {
    case _ ~ t ~ _ => t
  }

  private lazy val typeId: P[TypeId] = upper.map {
    case UpperWordToken(str, pos) => TypeId(str, pos)
  }

  private lazy val topType: P[TopType] = kw(TopKw).map(top => TopType(top.pos))

  private lazy val depType: P[DepType] = (kw(DepKw) ~ openParenth ~ identifier ~ colon ~ tpe ~ closeParenth ~ tpe).map {
    case dep ~ _ ~ param ~ _ ~ paramType ~ _ ~ bodyType => DepType(param, paramType, bodyType, dep.pos)
  }

  private lazy val boxType: P[BoxType] = (kw(BoxKw) ~ tpe).map {
    case box ~ boxed => BoxType(boxed, box.pos)
  }

  private lazy val unitType: P[UnitType] = kw(UnitKw).map(unit => UnitType(unit.pos))

  private lazy val refType: P[RefType] = (kw(RefKw) ~ typeShape).map {
    case ref ~ referenced => RefType(referenced, ref.pos)
  }

  private lazy val regType: P[RegType] = kw(RegKw).map(reg => RegType(reg.pos))

  private lazy val recordType: P[RecordType] =
    (opt(kw(SelfKw) ~ identifier ~ kw(InKw)) ~ openBrace ~ rep(identifier ~ colon ~ tpe) ~ closeBrace).map {
      case idToksOpt ~ openB ~ fieldsToks ~ _ => {
        val idOpt = idToksOpt.map {
          case _ ~ id ~ _ => id
        }
        val fields = fieldsToks.map {
          case fldName ~ _ ~ fldType => (fldName, fldType)
        }
        val pos = idOpt.map(_.position).getOrElse(openB.pos)
        RecordType(fields, idOpt, pos)
      }
    }

  private lazy val typeShape: P[TypeShape] = topType ||| depType ||| boxType ||| unitType ||| refType ||| regType ||| recordType

  private lazy val tpe: P[Type] = (typeShape ~ opt(op(Hat) ~ opt(explicitCaptureSet))).map {
    case shape ~ Some(_ ~ Some(explicitCapSet)) =>
      Type(shape, Some(explicitCapSet), shape.position)
    case shape ~ Some(hat ~ None) =>
      Type(shape, Some(ImplicitCaptureSet(hat.pos)), shape.position)
    case shape ~ None =>
      Type(shape, None, shape.position)
  }

  private lazy val explicitCaptureSet: P[ExplicitCaptureSet] = (openBrace ~ repsep(path, comma) ~ closeBrace).map {
    case openB ~ capPaths ~ _ => ExplicitCaptureSet(capPaths, openB.pos)
  }


  override protected def runImpl(in: Seq[GradCCToken], reporter: Reporter): Term = {
    val interestingTokens = filterIsKindedToken(in)
    val reader = ParsingReader.from(interestingTokens)
    (term ~ eof).apply(reader) match {
      case Success(result ~ (), remaining) =>
        result
      case Failure(msg, remaining) => reporter.fatal(msg, remaining.first.pos)
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

  case class ParsingException(msg: String) extends RuntimeException(msg)

}
