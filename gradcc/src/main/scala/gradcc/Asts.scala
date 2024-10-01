package gradcc

import commons.Position

type VarId = String
type FieldId = String

sealed trait Ast {
  val position: Position
}

sealed trait Expr extends Ast

sealed trait Variable extends Expr
final case class Identifier(varId: VarId, override val position: Position) extends Variable
final case class Cap(override val position: Position) extends Variable

sealed trait Path extends Expr
final case class Select(lhs: Expr, override val position: Position) extends Path
final case class RegSelect(override val position: Position) extends Path

final case class Box(boxed: Expr, override val position: Position) extends Expr
final case class Abs(varId: VarId, tpe: Type, body: Expr, override val position: Position) extends Expr
final case class RecordLiteral(fields: Seq[(FieldId, Expr)], override val position: Position) extends Expr
final case class UnitLiteral(override val position: Position) extends Expr

final case class App(callee: Expr, arg: Expr, override val position: Position) extends Expr
final case class Unbox(captureSet: CaptureSet, boxed: Expr, override val position: Position) extends Expr
final case class Let(varId: VarId, boundExpr: Expr, body: Expr, override val position: Position) extends Expr
final case class Region(override val position: Position) extends Expr
final case class Deref(ref: Expr, override val position: Position) extends Expr
final case class Assign(ref: Expr, newVal: Expr, override val position: Position) extends Expr



sealed trait Type extends Ast

sealed trait CaptureSet extends Ast
