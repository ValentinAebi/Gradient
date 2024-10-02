package commons.parsing

import commons.Reporter

def rep[Tok <: KindedToken, I](r: TreeParser[Tok, I]): TreeParser[Tok, List[I]] = RepParser(r)

trait TreeParser[Tok <: KindedToken, A] {
  a =>

  val descr: String

  def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean

  def consume(iterator: ParsingIterator[Tok], nextAdmits: => Boolean, reporter: Reporter): A

  override def toString: String = descr

  infix def ~[B](b: TreeParser[Tok, B]): TreeParser[Tok, A ~ B] = ConcatParser(a, b)

  infix def |[B](b: TreeParser[Tok, B]): TreeParser[Tok, A | B] = OrParser(a, b)

  infix def opt: TreeParser[Tok, Option[A]] = OptParser(a)

  def first: Set[TokenKind]

  def mayBeTransparent: Boolean

}

final class LeafParser[Tok <: KindedToken, Result](tokenKind: TokenKind)(f: Tok => Result) extends TreeParser[Tok, Result] {

  override val descr: String = tokenKind.toString

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    iterator.current.kind == tokenKind

  override def consume(iterator: ParsingIterator[Tok], nextAdmits: => Boolean, reporter: Reporter): Result = {
    val tok = iterator.current
    if (tok.kind != tokenKind) {
      val token = iterator.current
      reporter.fatal(s"unexpected token: '$token'", token.pos)
    }
    iterator.move(reporter)
    f(tok)
  }

  override def first: Set[TokenKind] = Set(tokenKind)

  override def mayBeTransparent: Boolean = false
}

final class ConcatParser[Tok <: KindedToken, L, R](l: TreeParser[Tok, L], r: TreeParser[Tok, R])
  extends TreeParser[Tok, L ~ R] {

  override val descr: String = s"${maybeParenthDescr(l)} ~ ${maybeParenthDescr(r)}"

  require(!(l.mayBeTransparent && l.first.intersect(r.first).nonEmpty), s"Not LL1: $descr")

  private def maybeParenthDescr(p: TreeParser[?, ?]): String = p match {
    case _: OrParser[?, ?, ?] => s"(${p.descr})"
    case _ => p.descr
  }

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    l.admits(iterator, r.admits(iterator, nextAdmits))

  override def consume(iterator: ParsingIterator[Tok], nextAdmits: => Boolean, reporter: Reporter): L ~ R = {
    val lRes = l.consume(iterator, r.admits(iterator, nextAdmits), reporter)
    val rRes = r.consume(iterator, nextAdmits, reporter)
    new~(lRes, rRes)
  }

  override def first: Set[TokenKind] = if l.mayBeTransparent then l.first ++ r.first else l.first

  override def mayBeTransparent: Boolean = l.mayBeTransparent && r.mayBeTransparent
}

final class OrParser[Tok <: KindedToken, L, R](l: TreeParser[Tok, L], r: TreeParser[Tok, R])
  extends TreeParser[Tok, L | R] {

  override val descr: String = s"${l.descr} | ${r.descr}"

  require(l.first.intersect(r.first).isEmpty, s"Not LL1: $descr")

  override def admits(iterator: ParsingIterator[Tok], nextAdmitsByName: => Boolean): Boolean = {
    lazy val nextAdmitsLazy = nextAdmitsByName
    l.admits(iterator, nextAdmitsLazy) || r.admits(iterator, nextAdmitsLazy)
  }

  override def consume(iterator: ParsingIterator[Tok], nextAdmitsByName: => Boolean, reporter: Reporter): L | R = {
    lazy val nextAdmitsLazy = nextAdmitsByName
    val lAdmits = l.admits(iterator, nextAdmitsLazy)
    val rAdmits = r.admits(iterator, nextAdmitsLazy)
    assert(!lAdmits || !rAdmits, s"Not LL1: $descr")
    if lAdmits then l.consume(iterator, nextAdmitsLazy, reporter) else r.consume(iterator, nextAdmitsLazy, reporter)
  }

  override def first: Set[TokenKind] = l.first ++ r.first

  override def mayBeTransparent: Boolean = l.mayBeTransparent || r.mayBeTransparent
}

final class OptParser[Tok <: KindedToken, O](opt: TreeParser[Tok, O]) extends TreeParser[Tok, Option[O]] {
  require(!opt.mayBeTransparent)

  override val descr: String = s"[${opt.descr}]"

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    opt.admits(iterator, nextAdmits) || nextAdmits

  override def consume(iterator: ParsingIterator[Tok], nextAdmits: => Boolean, reporter: Reporter): Option[O] = {
    val optAdmits = opt.admits(iterator, nextAdmits)
    assert(!optAdmits || !nextAdmits, s"Not LL1: $descr")
    if optAdmits then Some(opt.consume(iterator, nextAdmits, reporter)) else None
  }

  override def first: Set[TokenKind] = opt.first

  override def mayBeTransparent: Boolean = true
}

final class RepParser[Tok <: KindedToken, I](repeated: TreeParser[Tok, I]) extends TreeParser[Tok, List[I]] {
  require(!repeated.mayBeTransparent)

  override val descr: String = s"rep { $repeated }"

  override def admits(iterator: ParsingIterator[Tok], nextAdmits: => Boolean): Boolean =
    repeated.admits(iterator, nextAdmits)

  override def consume(iterator: ParsingIterator[Tok], nextAdmits: => Boolean, reporter: Reporter): List[I] = {
    val repAdmits = repeated.admits(iterator, nextAdmits)
    require(!repAdmits || !nextAdmits)
    val lsBuilder = List.newBuilder[I]
    while (repeated.admits(iterator, nextAdmits)){
      val iterRes = repeated.consume(iterator, nextAdmits, reporter)
      lsBuilder.addOne(iterRes)
    }
    lsBuilder.result()
  }

  override def first: Set[TokenKind] = repeated.first

  override def mayBeTransparent: Boolean = true
}

final infix case class ~[L, R](l: L, r: R) {
  override def toString: String = s"$l ~ $r"
}
