package gradcc.asts

import gradcc.lang.Type
import gradcc.reporting.Position

trait TermsProvider {
  
  type VarId
  type R[+T <: TermTree]

  def str(varId: VarId): String
  def getTerm[T <: TermTree](r: R[T]): T
  def getType[T <: TermTree](r: R[T]): Option[Type]
  
  val hasTypes: Boolean

  sealed trait Ast {
    val position: Position
    def description: String = getClass.getSimpleName.toLowerCase()
  }

  sealed trait TermTree extends Ast

  sealed trait VariableTree extends TermTree
  sealed trait StablePathTree extends TermTree
  sealed trait ProperPathTree extends StablePathTree
  case class IdentifierTree(id: VarId, override val position: Position) extends VariableTree, ProperPathTree {
    override def description: String = str(id)
  }
  case class CapTree(override val position: Position) extends VariableTree
  case class SelectTree(lhs: R[ProperPathTree], field: FieldTree, override val position: Position) extends ProperPathTree {
    override def description: String = s".$field"
  }
  case class BrandedPathTree(properPath: R[ProperPathTree], override val position: Position) extends StablePathTree

  sealed trait FieldTree extends Ast
  case class NamedFieldTree(fieldName: String, override val position: Position) extends FieldTree
  case class RegFieldTree(override val position: Position) extends FieldTree

  sealed trait ValueTree extends TermTree
  case class BoxTree(
                      boxed: R[StablePathTree],
                      override val position: Position
                    ) extends ValueTree
  case class AbsTree(
                      varId: IdentifierTree,
                      tpe: TypeTree,
                      body: R[TermTree],
                      override val position: Position
                    ) extends ValueTree
  case class RecordLiteralTree(fields: Seq[(FieldTree, R[StablePathTree])], override val position: Position) extends ValueTree
  case class UnitLiteralTree(override val position: Position) extends ValueTree
  
  case class AppTree(callee: R[StablePathTree], arg: R[StablePathTree], override val position: Position) extends TermTree
  case class UnboxTree(
                        captureSet: CaptureDescriptorTree,
                        boxed: R[StablePathTree],
                        override val position: Position
                      ) extends TermTree
  case class LetTree(
                      varId: IdentifierTree,
                      value: R[TermTree],
                      typeAnnot: Option[TypeTree],
                      body: R[TermTree],
                      override val position: Position
                    ) extends TermTree
  case class RegionTree(override val position: Position) extends TermTree
  case class DerefTree(ref: R[StablePathTree], override val position: Position) extends TermTree
  case class AssignTree(
                         ref: R[StablePathTree],
                         newVal: R[StablePathTree],
                         override val position: Position
                       ) extends TermTree
  case class RefTree(
                      regionCap: R[StablePathTree],
                      initVal: R[StablePathTree],
                      override val position: Position
                    ) extends TermTree
  case class ModuleTree(
                         regionCap: R[StablePathTree],
                         fields: Seq[(FieldTree, R[StablePathTree])],
                         override val position: Position
                       ) extends TermTree
  case class EnclosureTree(
                            permissions: CaptureSetTree,
                            tpe: TypeTree,
                            body: R[TermTree],
                            override val position: Position
                          ) extends TermTree
  case class ObscurTree(
                         obscured: R[StablePathTree],
                         varId: IdentifierTree,
                         body: R[TermTree],
                         override val position: Position
                       ) extends TermTree
  
  case class TypeTree(
                       shape: ShapeTree,
                       captureSet: Option[CaptureDescriptorTree],
                       override val position: Position
                     ) extends Ast

  sealed trait ShapeTree extends Ast
  case class TopShapeTree(override val position: Position) extends ShapeTree
  case class AbsShapeTree(
                           varId: IdentifierTree,
                           varType: TypeTree,
                           resType: TypeTree,
                           override val position: Position
                         ) extends ShapeTree
  case class BoxShapeTree(boxedType: TypeTree, override val position: Position) extends ShapeTree
  case class UnitShapeTree(override val position: Position) extends ShapeTree
  case class RefShapeTree(referencedType: ShapeTree, override val position: Position) extends ShapeTree
  case class RegShapeTree(override val position: Position) extends ShapeTree
  case class RecordShapeTree(
                              selfRef: Option[IdentifierTree],
                              fieldsInOrder: Seq[(FieldTree, TypeTree)],
                              override val position: Position
                            ) extends ShapeTree
  
  sealed trait CaptureDescriptorTree extends Ast
  sealed trait CaptureSetTree extends CaptureDescriptorTree
  case class NonRootCaptureSetTree(
                                    capturedVarsInOrder: Seq[R[ProperPathTree]],
                                    override val position: Position
                                  ) extends CaptureSetTree
  case class RootCaptureSetTree(override val position: Position) extends CaptureSetTree
  case class BrandDescriptorTree(override val position: Position) extends CaptureDescriptorTree
  
}
