package commons.parsing

import commons.{Position, Reporter}

def rep[Tok <: KindedToken, R](repeated: TreeParser[Tok, R]): TreeParser[Tok, List[R]] = RepParser(repeated)

def opt[Tok <: KindedToken, A](optional: TreeParser[Tok, A]): TreeParser[Tok, Option[A]] = OptParser(optional)

def rep1[Tok <: KindedToken, R](repeated: TreeParser[Tok, R]): TreeParser[Tok, R ~ List[R]] = repeated ~ rep(repeated)

def rep1Ls[Tok <: KindedToken, R](repeated: TreeParser[Tok, R]): TreeParser[Tok, List[R]] =
  rep1(repeated).map {
    case h ~ t => h :: t
  }

def rep1WithSep[Tok <: KindedToken, R](repeated: TreeParser[Tok, R], separator: TreeParser[Tok, ?]): TreeParser[Tok, R ~ List[R]] =
  repeated ~ rep(separator ~ repeated).map(_.map(_.r))

def rep1WithSepLs[Tok <: KindedToken, R](repeated: TreeParser[Tok, R], separator: TreeParser[Tok, ?]): TreeParser[Tok, List[R]] =
  (repeated ~ rep(separator ~ repeated)).map {
    case r ~ ls => r :: ls.map(_.r)
  }

def repWithSep[Tok <: KindedToken, R](repeated: TreeParser[Tok, R], separator: TreeParser[Tok, ?]): TreeParser[Tok, List[R]] =
  opt(rep1WithSepLs(repeated, separator)).map(_.getOrElse(Nil))

extension [Tok <: KindedToken, A](p: => TreeParser[Tok, A]) def asLazy: TreeParser[Tok, A] =
  LazyTreeParser(p)

trait TreeParser[Tok <: KindedToken, +A] {
  a =>

  def descr: String

  def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean

  def consume(iterator: ParsingIterator[Tok], reporter: Reporter, nextAdmits: => Boolean): A

  override def toString: String = descr

  infix def ~[B](b: TreeParser[Tok, B]): TreeParser[Tok, A ~ B] = ConcatParser(a, b)

  infix def |[B](b: TreeParser[Tok, B]): TreeParser[Tok, A | B] = OrParser(a, b)

  def map[B](f: Position ?=> A => B): TreeParser[Tok, B] = MapParser(a, f)

  def first: Set[TokenKind[?]]

  def mayBeTransparent: Boolean

}

final class LeafParser[Tok <: KindedToken, ResTok <: Tok](tokenKind: TokenKind[ResTok])
  extends TreeParser[Tok, ResTok] {

  override def descr: String = tokenKind.toString

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    iterator.current.kind == tokenKind

  override def consume(iterator: ParsingIterator[Tok], reporter: Reporter, nextAdmits: => Boolean): ResTok = {
    val token = iterator.current
    tokenKind.ifMatchesOrElse(token) { token =>
      iterator.move(reporter)
      token
    } {
      reporter.fatal(s"unexpected token: '$token'", token.pos)
    }
  }

  override def first: Set[TokenKind[?]] = Set(tokenKind)

  override def mayBeTransparent: Boolean = false
}

final class ConcatParser[Tok <: KindedToken, L, R](l: TreeParser[Tok, L], r: TreeParser[Tok, R])
  extends TreeParser[Tok, L ~ R] {
  if (l.mayBeTransparent) {
    requireDisjointFirsts(l, r, descr)
  }

  override def descr: String = s"${maybeParenthDescr(l)} ~ ${maybeParenthDescr(r)}"

  private def maybeParenthDescr(p: TreeParser[?, ?]): String = p match {
    case _: OrParser[?, ?, ?] => s"(${p.descr})"
    case _ => p.descr
  }

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    l.admits(iterator, r.admits(iterator, nextAdmits))

  override def consume(iterator: ParsingIterator[Tok], reporter: Reporter, nextAdmits: => Boolean): L ~ R = {
    val lRes = l.consume(iterator, reporter, r.admits(iterator, nextAdmits))
    val rRes = r.consume(iterator, reporter, nextAdmits)
    new~(lRes, rRes)
  }

  override def first: Set[TokenKind[?]] = if l.mayBeTransparent then l.first ++ r.first else l.first

  override def mayBeTransparent: Boolean = l.mayBeTransparent && r.mayBeTransparent
}

final class OrParser[Tok <: KindedToken, L, R](l: TreeParser[Tok, L], r: TreeParser[Tok, R])
  extends TreeParser[Tok, L | R] {
  requireDisjointFirsts(l, r, descr)

  override def descr: String = s"${l.descr} | ${r.descr}"

  override def admits(iterator: ParsingIterator[Tok], nextAdmitsByName: => Boolean): Boolean = {
    lazy val nextAdmitsLazy = nextAdmitsByName
    l.admits(iterator, nextAdmitsLazy) || r.admits(iterator, nextAdmitsLazy)
  }

  override def consume(iterator: ParsingIterator[Tok], reporter: Reporter, nextAdmitsByName: => Boolean): L | R = {
    lazy val nextAdmitsLazy = nextAdmitsByName
    val lAdmits = l.admits(iterator, nextAdmitsLazy)
    val rAdmits = r.admits(iterator, nextAdmitsLazy)
    assert(!lAdmits || !rAdmits, s"Not LL1: $descr")
    (if lAdmits then l else r).consume(iterator, reporter, nextAdmitsLazy)
  }

  override def first: Set[TokenKind[?]] = l.first ++ r.first

  override def mayBeTransparent: Boolean = l.mayBeTransparent || r.mayBeTransparent
}

final class OptParser[Tok <: KindedToken, O](opt: TreeParser[Tok, O]) extends TreeParser[Tok, Option[O]] {
  require(!opt.mayBeTransparent)

  override def descr: String = s"[${opt.descr}]"

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    opt.admits(iterator, nextAdmits) || nextAdmits

  override def consume(iterator: ParsingIterator[Tok], reporter: Reporter, nextAdmits: => Boolean): Option[O] = {
    val optAdmits = opt.admits(iterator, nextAdmits)
    assert(!optAdmits || !nextAdmits, s"Not LL1: $descr")
    if optAdmits then Some(opt.consume(iterator, reporter, nextAdmits)) else None
  }

  override def first: Set[TokenKind[?]] = opt.first

  override def mayBeTransparent: Boolean = true
}

final class RepParser[Tok <: KindedToken, I](repeated: TreeParser[Tok, I]) extends TreeParser[Tok, List[I]] {
  require(!repeated.mayBeTransparent)

  override def descr: String = s"rep { $repeated }"

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    repeated.admits(iterator, nextAdmits)

  override def consume(iterator: ParsingIterator[Tok], reporter: Reporter, nextAdmits: => Boolean): List[I] = {
    val repAdmits = repeated.admits(iterator, nextAdmits)
    require(!repAdmits || !nextAdmits)
    val lsBuilder = List.newBuilder[I]
    while (repeated.admits(iterator, nextAdmits)) {
      val iterRes = repeated.consume(iterator, reporter, nextAdmits)
      lsBuilder.addOne(iterRes)
    }
    lsBuilder.result()
  }

  override def first: Set[TokenKind[?]] = repeated.first

  override def mayBeTransparent: Boolean = true
}

final class MapParser[Tok <: KindedToken, A, B](baseParser: TreeParser[Tok, A], mapF: Position ?=> A => B) extends TreeParser[Tok, B] {
  override def descr: String = baseParser.descr

  export baseParser.{admits, first, mayBeTransparent}

  override def consume(iterator: ParsingIterator[Tok], reporter: Reporter, nextAdmits: => Boolean): B = {
    val pos = iterator.current.pos
    val a = baseParser.consume(iterator, reporter, nextAdmits)
    mapF(using pos)(a)
  }

}

final class EofParser[Tok <: KindedToken](eofKind: TokenKind[?]) extends TreeParser[Tok, Unit] {
  override def descr: String = "<end-of-file>"

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    eofKind.matches(iterator.current)

  override def consume(iterator: ParsingIterator[Tok], reporter: Reporter, nextAdmits: => Boolean): Unit = ()

  override def first: Set[TokenKind[?]] = Set(eofKind)

  override def mayBeTransparent: Boolean = false
}

final class LazyTreeParser[Tok <: KindedToken, A](baseParser: => TreeParser[Tok, A]) extends TreeParser[Tok, A] {
  private lazy val baseParserComputeOnce = baseParser
  export baseParserComputeOnce.*
}

private def requireDisjointFirsts(l: TreeParser[?, ?], r: TreeParser[?, ?], descr: => String): Unit = {
  val inter = l.first.intersect(r.first)
  if (inter.nonEmpty){
    throw IllegalAccessException(s"Parser $descr is not LL1. Conflicts found involving the following kinds: $inter")
  }
}

final infix case class ~[+L, +R](l: L, r: R) {
  override def toString: String = s"$l ~ $r"
}
