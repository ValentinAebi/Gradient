package gradcc.asts

import commons.Position

trait TermsProvider {

  private val disambiguationMarker: Char = '$'
  
  type VarId

  sealed trait Term extends Ast

  sealed trait Variable extends Term
  sealed trait Path extends Term
  case class Identifier(id: VarId, override val position: Position) extends Variable, Path
  case class Cap(override val position: Position) extends Variable
  case class Select(root: Path, select: String, override val position: Position) extends Path

  sealed trait Field extends Ast
  case class NamedField(fieldName: String, override val position: Position) extends Field
  case class Reg(override val position: Position) extends Field

  sealed trait Value extends Term
  case class Box(boxed: Path, override val position: Position) extends Value
  case class Abs(varId: Identifier, tpe: TypeTree, body: Term, override val position: Position) extends Value
  case class RecordLiteral(fields: Seq[(Field, Path)], selfRef: Option[Identifier], override val position: Position) extends Value
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
  case class RecordTypeTree(fieldsInOrder: Seq[(NamedField, TypeTree)], selfRef: Option[Identifier], override val position: Position) extends TypeShapeTree

  sealed trait CaptureSetTree extends Ast
  case class ExplicitCaptureSetTree(capturedVarsInOrder: Seq[Path], override val position: Position) extends CaptureSetTree
  case class ImplicitCaptureSetTree(override val position: Position) extends CaptureSetTree
  
}
