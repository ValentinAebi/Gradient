package gradcc.phases.renaming

import gradcc.asts.{UniqueVarId, AmbiguouslyNamedTerms as A, UniquelyNamedTerms as U}
import gradcc.phases.SimplePhase
import gradcc.reporting.{Position, Reporter}

import scala.collection.mutable.Map as MutMap

final class RenamerPhase extends SimplePhase[A.TermTree, U.TermTree]("Renamer") {
  private val unknownId: UniqueVarId = UniqueVarId("<unknown>", -1)

  override val acceptsFaultyInput: Boolean = false

  override protected def runImpl(in: A.TermTree, reporter: Reporter): U.TermTree =
    convertTerm(in)(using Ctx(Map.empty, MutMap.empty, reporter))

  private def convertTerm(term: A.TermTree)(using ctx: Ctx): U.TermTree = term match {
    case path: A.StablePathTree =>
      convertStablePath(path)
    case A.CapTree(position) =>
      U.CapTree(position)
    case A.BoxTree(boxed, position) =>
      U.BoxTree(convertStablePath(boxed), position)
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
        fields.map((fld, p) => (convertField(fld), convertStablePath(p))),
        position
      )
    case A.UnitLiteralTree(position) =>
      U.UnitLiteralTree(position)
    case A.AppTree(callee, arg, position) =>
      U.AppTree(convertStablePath(callee), convertStablePath(arg), position)
    case A.UnboxTree(captureDescr, boxed, position) =>
      U.UnboxTree(convertCaptureDescriptor(captureDescr), convertStablePath(boxed), position)
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
      U.DerefTree(convertStablePath(ref), position)
    case A.AssignTree(ref, newVal, position) =>
      U.AssignTree(convertStablePath(ref), convertStablePath(newVal), position)
    case A.RefTree(regionCap, initVal, position) =>
      U.RefTree(convertStablePath(regionCap), convertStablePath(initVal), position)
    case A.ModuleTree(regionCap, fields, position) =>
      U.ModuleTree(
        convertStablePath(regionCap),
        fields.map((fld, p) => (convertField(fld), convertStablePath(p))),
        position
      )
    case A.EnclosureTree(permissions, tpe, body, position) =>
      U.EnclosureTree(
        convertCaptureSet(permissions),
        convertType(tpe),
        convertTerm(body),
        position
      )
    case A.ObscurTree(obscured, varId, body, position) =>
      val newCtx = ctx.withNewId(varId.id)
      U.ObscurTree(
        convertStablePath(obscured)(using ctx),
        convertIdentifier(varId)(using newCtx),
        convertTerm(body)(using newCtx),
        position
      )
  }
  
  private def convertStablePath(path: A.StablePathTree)(using ctx: Ctx): U.StablePathTree = path match {
    case properP: A.ProperPathTree => convertProperPath(properP)
    case A.BrandedPathTree(properPath, position) => U.BrandedPathTree(convertProperPath(properPath), position)
  }

  private def convertProperPath(path: A.ProperPathTree)(using ctx: Ctx): U.ProperPathTree = path match {
    case id: A.IdentifierTree => convertIdentifier(id)
    case A.SelectTree(root, fld, position) => U.SelectTree(convertProperPath(root), convertField(fld), position)
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
    U.TypeTree(convertShape(shape), capt.map(convertCaptureDescriptor), position)
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
  
  private def convertCaptureDescriptor(descr: A.CaptureDescriptorTree)(using ctx: Ctx): U.CaptureDescriptorTree = descr match {
    case cs: A.CaptureSetTree => convertCaptureSet(cs)
    case A.BrandDescriptorTree(position) => U.BrandDescriptorTree(position)
  }

  private def convertCaptureSet(capt: A.CaptureSetTree)(using ctx: Ctx): U.CaptureSetTree = capt match {
    case A.NonRootCaptureSetTree(capturedVarsInOrder, position) =>
      U.NonRootCaptureSetTree(capturedVarsInOrder.map(convertProperPath), position)
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
