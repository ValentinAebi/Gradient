package gradcc.parsing

import commons.parsing.*
import commons.{Position, Reporter, SimplePhase}
import gradcc.*
import gradcc.lang.Keyword.*
import gradcc.lang.Operator.*
import gradcc.lang.{Keyword, Operator}

import scala.collection.immutable.ArraySeq

class Parser extends SimplePhase[Seq[GradCCToken], Ast]("Parser") {

  // ----- Syntax primitives --------------------------

  private val lower: P[LowerWordToken] = LowerKind.parser
  private val upper: P[UpperWordToken] = UpperKind.parser
  private val eof = EofParser[KindedGradCCToken](EofKind)

  private def kw(kw: Keyword) = KeywordKind(kw).parser

  private def op(op: Operator) = OperatorKind(op).parser

  private val dot = op(Dot)
  private val comma = op(Comma)
  private val colon = op(Colon)
  private val equal = op(Equal)
  private val openParenth = op(OpenParenth)
  private val closeParenth = op(CloseParenth)
  private val openBrace = op(OpenBrace)
  private val closeBrace = op(CloseBrace)

  private type P[Result] = TreeParser[KindedGradCCToken, Result]

  // ----- Syntax -------------------------------------

  private lazy val identifier: P[Identifier] = lower.map {
    case LowerWordToken(str, pos) => Identifier(str, pos)
  }.asLazy

  private lazy val cap: P[Cap] = kw(CapKw).map(_ => Cap(startPos)).asLazy
  private lazy val variable: P[Variable] = (identifier | cap).asLazy

  private lazy val reg: P[Reg] = (kw(RegKw).map(_ => Reg(startPos))).asLazy
  private lazy val field: P[Field] = (identifier | reg).asLazy

  private lazy val path: P[Path] = rep1WithSep(identifier, dot).map {
    case h ~ Nil => h
    case h ~ t => CompoundPath(h, t, startPos)
  }.asLazy

  private lazy val box: P[Box] = (kw(BoxKw) ~ path).map {
    case _ ~ p => Box(p, startPos)
  }.asLazy

  private lazy val abs: P[Abs] = (kw(FnKw) ~ rep1(openParenth ~ identifier ~ colon ~ tpe ~ closeParenth) ~ term).map {
    case _ ~ ((_ ~ param1Id ~ _ ~ param1Type ~ _) ~ subSequentParams) ~ body => {
      val abs1Body = subSequentParams.foldRight(body) {
        (param, accBody) => {
          val _ ~ id ~ _ ~ tpe ~ _ = param
          Abs(id, tpe, accBody, id.position)
        }
      }
      Abs(param1Id, param1Type, abs1Body, startPos)
    }
  }.asLazy

  private lazy val recordLit: P[RecordLiteral] = (openBrace ~ rep(field ~ equal ~ path) ~ closeBrace).map {
    case _ ~ fields ~ _ => RecordLiteral(fields.map {
      case fld ~ _ ~ p => (fld, p)
    }, startPos)
  }.asLazy

  private lazy val unitLit: P[UnitLiteral] = (openParenth ~ closeParenth).map(_ => UnitLiteral(startPos)).asLazy

  private lazy val value: P[Value] = (box | abs | recordLit | unitLit).asLazy

  private lazy val unbox: P[Unbox] = (captureSet ~ kw(UnboxKw) ~ path).map {
    case capSet ~ _ ~ p => Unbox(capSet, p, startPos)
  }.asLazy

  private lazy val let: P[Let] = (kw(LetKw) ~ identifier ~ equal ~ term ~ kw(InKw) ~ term).map {
    case _ ~ id ~ _ ~ value ~ _ ~ body => Let(id, value, body, startPos)
  }.asLazy

  private lazy val region: P[Region] = kw(RegionKw).map(_ => Region(startPos)).asLazy

  private lazy val deref: P[Deref] = (op(Bang) ~ path).map {
    case _ ~ p => Deref(p, startPos)
  }.asLazy

  private lazy val modif: P[Modif] =
    (kw(ModKw) ~ openParenth ~ path ~ closeParenth ~ openBrace ~ rep1WithSepLs(field ~ equal ~ path, comma) ~ closeBrace).map {
      case _ ~ _ ~ capRegion ~ _ ~ _ ~ fields ~ _ =>
        Modif(capRegion, fields.map {
          case id ~ _ ~ p => (id, p)
        }, startPos)
    }.asLazy

  private lazy val termStartingWithPath: P[Term] =
    (path ~ opt((colon ~ equal ~ path) | (kw(RefKw) ~ path) | rep1(path))).map {
      case p ~ None => p
      case lhs ~ Some(OperatorToken(Colon, _) ~ OperatorToken(Equal, _) ~ (rhs: Path)) =>
        Assign(lhs, rhs, startPos)
      case capReg ~ Some(_ ~ (initVal: Path)) =>
        Ref(capReg, initVal, startPos)
      case fst ~ Some((snd: Path) ~ (rem: List[Path])) =>
        rem.foldLeft(App(fst, snd, startPos)) { (accApp, nextArg) =>
          App(accApp, nextArg, startPos) // TODO maybe better to report error on the argument?
        }
    }.asLazy

  private lazy val term: P[Term] = (termStartingWithPath | cap | value | unbox | let | region | deref | modif).asLazy

  private lazy val typeId: P[TypeId] = upper.map {
    case UpperWordToken(str, pos) => TypeId(str, pos)
  }.asLazy

  private lazy val topType: P[TopType] = kw(TopKw).map(_ => TopType(startPos)).asLazy

  private lazy val depType: P[DepType] = (kw(DepKw) ~ openParenth ~ identifier ~ colon ~ tpe ~ closeParenth ~ tpe).map {
    case _ ~ _ ~ param ~ _ ~ paramType ~ _ ~ bodyType => DepType(param, paramType, bodyType, startPos)
  }.asLazy

  private lazy val boxType: P[BoxType] = ???

  private lazy val unitType: P[UnitType] = kw(UnitKw).map(_ => UnitType(startPos)).asLazy

  private lazy val refType: P[RefType] = ???

  private lazy val regType: P[RegType] = ???

  private lazy val recordType: P[RecordType] = ???

  private lazy val typeShape: P[TypeShape] = (topType | depType | boxType | unitType | refType | regType | recordType).asLazy

  private lazy val tpe: P[Type] = (typeShape ~ op(Hat) ~ opt(captureSet)).map {
    case shape ~ hat ~ capSetOpt => Type(shape, capSetOpt.getOrElse(CaptureSet(Seq.empty, hat.pos)), startPos)
  }.asLazy

  private lazy val captureSet: P[CaptureSet] = (openBrace ~ repWithSep(path, comma) ~ closeBrace).map {
    case _ ~ capPaths ~ _ => CaptureSet(capPaths, startPos)
  }.asLazy

  override protected def runImpl(in: Seq[GradCCToken], reporter: Reporter): Ast = {
    val interestingTokens = filterIsKindedToken(in)
    val iter = ParsingIterator[KindedGradCCToken](interestingTokens)

    val syntax = abs ~ eof
    val r = syntax.consume(iter, reporter, assert(false))
    println(r)

    // TODO remove this
    null
  }

  private def startPos(using Position): Position = summon

  private def filterIsKindedToken(tokens: Seq[GradCCToken]): ArraySeq[KindedGradCCToken] = {
    val b = ArraySeq.newBuilder[KindedGradCCToken]
    tokens foreach {
      case kindedGradCCToken: KindedGradCCToken =>
        b.addOne(kindedGradCCToken)
      case _ => ()
    }
    b.result()
  }

}
