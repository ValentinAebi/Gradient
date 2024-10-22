package gradcc.renaming

import gradcc.asts.{UniqueVarId, AmbiguouslyNamedTerms as A, UniquelyNamedTerms as U}
import gradcc.{Position, Reporter, SimplePhase}

import scala.collection.mutable.Map as MutMap

final class RenamerPhase extends SimplePhase[A.TermTree, U.TermTree]("Renamer") {
  private val unknownId: UniqueVarId = UniqueVarId("<unknown>", -1)

  override val acceptsFaultyInput: Boolean = false

  override protected def runImpl(in: A.TermTree, reporter: Reporter): U.TermTree =
    convertTerm(in)(using Ctx(Map.empty, MutMap.empty, reporter))

  private def convertTerm(term: A.TermTree)(using ctx: Ctx): U.TermTree = term match {
    case path: A.PathTree =>
      convertPath(path)
    case A.CapTree(position) =>
      U.CapTree(position)
    case A.BoxTree(boxed, position) =>
      U.BoxTree(convertPath(boxed), position)
    case A.AbsTree(varId, varType, body, position) =>
      val newCtx = ctx.withNewId(varId.id)
      U.AbsTree(
        convertIdentifier(varId)(using newCtx),
        convertType(varType)(using ctx),
        convertTerm(body)(using newCtx),
        position
      )
    case A.RecordLiteralTree(fields, position) =>
      U.RecordLiteralTree(
        fields.map((fld, p) => (convertField(fld), convertPath(p))),
        position
      )
    case A.UnitLiteralTree(position) =>
      U.UnitLiteralTree(position)
    case A.AppTree(callee, arg, position) =>
      U.AppTree(convertPath(callee), convertPath(arg), position)
    case A.UnboxTree(captureSet, boxed, position) =>
      U.UnboxTree(convertCaptureSetTree(captureSet), convertPath(boxed), position)
    case A.LetTree(varId, value, typeAnnot, body, position) =>
      val newCtx = ctx.withNewId(varId.id)
      U.LetTree(
        convertIdentifier(varId)(using newCtx),
        convertTerm(value)(using ctx),
        typeAnnot.map(convertType),
        convertTerm(body)(using newCtx),
        position
      )
    case A.RegionTree(position) =>
      U.RegionTree(position)
    case A.DerefTree(ref, position) =>
      U.DerefTree(convertPath(ref), position)
    case A.AssignTree(ref, newVal, position) =>
      U.AssignTree(convertPath(ref), convertPath(newVal), position)
    case A.RefTree(regionCap, initVal, position) =>
      U.RefTree(convertPath(regionCap), convertPath(initVal), position)
    case A.ModuleTree(regionCap, fields, position) =>
      U.ModuleTree(
        convertPath(regionCap),
        fields.map((fld, p) => (convertField(fld), convertPath(p))),
        position
      )
  }

  private def convertPath(path: A.PathTree)(using ctx: Ctx): U.PathTree = path match {
    case id: A.IdentifierTree => convertIdentifier(id)
    case A.SelectTree(root, fld, position) => U.SelectTree(convertPath(root), convertField(fld), position)
  }

  private def convertIdentifier(ident: A.IdentifierTree)(using ctx: Ctx): U.IdentifierTree = {
    val A.IdentifierTree(id, position) = ident
    U.IdentifierTree(ctx.getCurrentIdFor(id, position), position)
  }

  private def convertField(fld: A.FieldTree): U.FieldTree = fld match {
    case A.NamedFieldTree(fieldName, position) => U.NamedFieldTree(fieldName, position)
    case A.RegFieldTree(position) => U.RegFieldTree(position)
  }

  private def convertType(tpe: A.TypeTree)(using ctx: Ctx): U.TypeTree = {
    val A.TypeTree(shape, capt, position) = tpe
    U.TypeTree(convertShape(shape), capt.map(convertCaptureSetTree), position)
  }

  private def convertShape(shape: A.ShapeTree)(using ctx: Ctx): U.ShapeTree = shape match {
    case A.TopShapeTree(position) =>
      U.TopShapeTree(position)
    case A.AbsShapeTree(varId, varType, bodyType, position) =>
      val newCtx = ctx.withNewId(varId.id)
      U.AbsShapeTree(
        convertIdentifier(varId)(using newCtx),
        convertType(varType)(using ctx),
        convertType(bodyType)(using newCtx),
        position
      )
    case A.BoxShapeTree(boxedType, position) =>
      U.BoxShapeTree(convertType(boxedType), position)
    case A.UnitShapeTree(position) =>
      U.UnitShapeTree(position)
    case A.RefShapeTree(referencedType, position) =>
      U.RefShapeTree(convertShape(referencedType), position)
    case A.RegShapeTree(position) =>
      U.RegShapeTree(position)
    case A.RecordShapeTree(selfRef, fieldsInOrder, position) =>
      val updatedCtx = selfRef.map(selfRef => ctx.withNewId(selfRef.id)).getOrElse(ctx)
      U.RecordShapeTree(
        selfRef.map(convertIdentifier(_)(using updatedCtx)),
        fieldsInOrder.map((fld, tpe) => (convertField(fld), convertType(tpe)(using updatedCtx))),
        position
      )
  }

  private def convertCaptureSetTree(capt: A.CaptureSetTree)(using ctx: Ctx): U.CaptureSetTree = capt match {
    case A.NonRootCaptureSetTree(capturedVarsInOrder, position) =>
      U.NonRootCaptureSetTree(capturedVarsInOrder.map(convertPath), position)
    case A.RootCaptureSetTree(position) =>
      U.RootCaptureSetTree(position)
  }

  private case class Ctx(
                          bindingsInScope: Map[String, UniqueVarId],
                          renamingIndices: MutMap[String, Int],
                          reporter: Reporter
                        ) {

    def withNewId(s: String): Ctx = {
      val idx = renamingIndices.getOrElse(s, 0)
      val newId = UniqueVarId(s, idx)
      renamingIndices(s) = idx + 1
      copy(bindingsInScope = bindingsInScope.updatedWith(s)(_ => Some(newId)))
    }

    def getCurrentIdFor(s: String, pos: Position): UniqueVarId = bindingsInScope.getOrElse(s, {
      reporter.error(s"not found: $s", pos)
      unknownId
    })

  }

}
