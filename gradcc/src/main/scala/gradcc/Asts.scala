package gradcc

import commons.Position

type VarId = String
type FieldId = String

sealed trait Ast {
  val position: Position
}

sealed trait Term extends Ast

sealed trait Variable extends Term
final case class Identifier(varId: VarId, override val position: Position) extends Variable
final case class Cap(override val position: Position) extends Variable

sealed trait Path extends Term
final case class Select(lhs: Term, override val position: Position) extends Path
final case class RegSelect(override val position: Position) extends Path

final case class Box(boxed: Term, override val position: Position) extends Term
final case class Abs(varId: VarId, tpe: Type, body: Term, override val position: Position) extends Term
final case class RecordLiteral(fields: Seq[(FieldId, Term)], override val position: Position) extends Term
final case class UnitLiteral(override val position: Position) extends Term

final case class App(callee: Term, arg: Term, override val position: Position) extends Term
final case class Unbox(captureSet: CaptureSet, boxed: Term, override val position: Position) extends Term
final case class Let(varId: VarId, boundExpr: Term, body: Term, override val position: Position) extends Term
final case class Region(override val position: Position) extends Term
final case class Deref(ref: Term, override val position: Position) extends Term
final case class Assign(ref: Term, newVal: Term, override val position: Position) extends Term


final case class Type(shape: TypeShape, captureSet: CaptureSet, override val position: Position) extends Ast

sealed trait TypeShape extends Ast
final case class TopType(override val position: Position) extends TypeShape
final case class DepType(varId: VarId, varType: Type, typeBody: Type, override val position: Position) extends TypeShape
final case class BoxType(boxedType: Type, override val position: Position) extends TypeShape
final case class UnitType(override val position: Position) extends TypeShape
final case class RefType(referencedType: TypeShape, override val position: Position) extends TypeShape
final case class RegType(override val position: Position) extends TypeShape
final case class RecordType(fields: Map[FieldId, Type], selfRef: Option[VarId], override val position: Position) extends TypeShape

final case class CaptureSet(capturedVarsInOrder: Seq[VarId], override val position: Position) extends Ast {
  val capturedVars: Set[VarId] = capturedVarsInOrder.toSet
}
