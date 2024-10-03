package gradcc

import commons.Position

sealed trait Ast {
  val position: Position
}

sealed trait Term extends Ast

sealed trait Variable extends Term
sealed trait Field extends Ast
sealed trait Path extends Term

final case class Identifier(id: String, override val position: Position) extends Variable, Field, Path
final case class Cap(override val position: Position) extends Variable
final case class Reg(override val position: Position) extends Field
final case class CompoundPath(owner: Identifier, pathParts: Seq[Identifier], override val position: Position) extends Path

sealed trait Value extends Term
final case class Box(boxed: Path, override val position: Position) extends Value
final case class Abs(varId: Identifier, tpe: Type, body: Term, override val position: Position) extends Value
final case class RecordLiteral(fields: Seq[(Field, Term)], override val position: Position) extends Value
final case class UnitLiteral(override val position: Position) extends Value

final case class App(callee: Term, arg: Term, override val position: Position) extends Term
final case class Unbox(captureSet: CaptureSet, boxed: Path, override val position: Position) extends Term
final case class Let(varId: Identifier, value: Term, body: Term, override val position: Position) extends Term
final case class Region(override val position: Position) extends Term
final case class Deref(ref: Path, override val position: Position) extends Term
final case class Assign(ref: Path, newVal: Path, override val position: Position) extends Term
final case class Ref(regionCap: Path, initVal: Path, override val position: Position) extends Term
final case class Modif(regionCap: Path, fields: Seq[(Field, Path)], override val position: Position) extends Term


final case class Type(shape: TypeShape, captureSet: CaptureSet, override val position: Position) extends Ast

sealed trait TypeShape extends Ast
final case class TypeId(id: String, override val position: Position) extends TypeShape
final case class TopType(override val position: Position) extends TypeShape
final case class DepType(varId: Identifier, varType: Type, typeBody: Type, override val position: Position) extends TypeShape
final case class BoxType(boxedType: Type, override val position: Position) extends TypeShape
final case class UnitType(override val position: Position) extends TypeShape
final case class RefType(referencedType: TypeShape, override val position: Position) extends TypeShape
final case class RegType(override val position: Position) extends TypeShape
final case class RecordType(fields: Map[Identifier, Type], selfRef: Option[Identifier], override val position: Position) extends TypeShape

final case class CaptureSet(capturedVarsInOrder: Seq[Path], override val position: Position) extends Ast {
  val capturedVars: Set[Path] = capturedVarsInOrder.toSet
}
