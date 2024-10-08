package gradcc.asts

import commons.Position
import gradcc.lang.Type

private val disambiguationMarker: Char = '$'

sealed trait Term extends Ast
sealed trait Variable extends Term
sealed trait Field extends Ast
sealed trait Path extends Term

case class Identifier(id: String, override val position: Position) extends Variable, Field, Path
case class Cap(override val position: Position) extends Variable
case class Reg(override val position: Position) extends Field
case class Select(root: Path, select: String, override val position: Position) extends Path

sealed trait Value extends Term
case class Box(boxed: Path, override val position: Position) extends Value
case class Abs(varId: Identifier, tpe: TypeTree, body: Term, override val position: Position) extends Value
case class RecordLiteral(fields: Seq[(Field, Path)], override val position: Position) extends Value
case class UnitLiteral(override val position: Position) extends Value

case class App(callee: Term, arg: Term, override val position: Position) extends Term
case class Unbox(captureSet: CaptureSetTree, boxed: Path, override val position: Position) extends Term
case class Let(varId: Identifier, value: Term, body: Term, override val position: Position) extends Term
case class Region(override val position: Position) extends Term
case class Deref(ref: Path, override val position: Position) extends Term
case class Assign(ref: Path, newVal: Path, override val position: Position) extends Term
case class Ref(regionCap: Path, initVal: Path, override val position: Position) extends Term
case class Modif(regionCap: Path, fields: Seq[(Field, Path)], override val position: Position) extends Term

case class TypeTree(shape: TypeShapeTree, captureSet: Option[CaptureSetTree], override val position: Position) extends Ast

sealed trait TypeShapeTree extends Ast
// TODO how to represent resources like Fs, Net, etc.?
case class TopTypeTree(override val position: Position) extends TypeShapeTree
case class AbsTypeTree(varId: Identifier, varType: TypeTree, bodyType: TypeTree, override val position: Position) extends TypeShapeTree
case class BoxTypeTree(boxedType: TypeTree, override val position: Position) extends TypeShapeTree
case class UnitTypeTree(override val position: Position) extends TypeShapeTree
case class RefTypeTree(referencedType: TypeShapeTree, override val position: Position) extends TypeShapeTree
case class RegTypeTree(override val position: Position) extends TypeShapeTree
case class RecordTypeTree(fieldsInOrder: Seq[(Identifier, TypeTree)], selfRef: Option[Identifier], override val position: Position) extends TypeShapeTree

sealed trait CaptureSetTree extends Ast
case class ExplicitCaptureSetTree(capturedVarsInOrder: Seq[Path], override val position: Position) extends CaptureSetTree
case class ImplicitCaptureSetTree(override val position: Position) extends CaptureSetTree
