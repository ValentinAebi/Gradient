package gradcc.renaming

import gradcc.{Position, Reporter, SimplePhase}
import gradcc.asts.{UniqueVarId, AmbiguouslyNamedTerms as A, UniquelyNamedTerms as U}

import scala.collection.mutable.Map as MutMap

final class RenamerPhase extends SimplePhase[A.Term, U.Term]("Renamer") {
  private val unknownId: UniqueVarId = UniqueVarId("<unknown>", -1)

  override protected def runImpl(in: A.Term, reporter: Reporter): U.Term =
    convertTerm(in)(using Ctx(Map.empty, MutMap.empty, reporter))
  
  private def convertTerm(term: A.Term)(using ctx: Ctx): U.Term = term match {
    case path: A.Path =>
      convertPath(path)
    case A.Cap(position) =>
      U.Cap(position)
    case A.Box(boxed, position) =>
      U.Box(convertPath(boxed), position)
    case A.Abs(varId, varType, body, position) =>
      val newCtx = ctx.withNewId(varId.id)
      U.Abs(
        convertIdentifier(varId)(using newCtx),
        convertType(varType)(using ctx),
        convertTerm(body)(using newCtx),
        position
      )
    case A.RecordLiteral(selfRef, fields, position) =>
      val updatedCtx = selfRef.map(selfRef => ctx.withNewId(selfRef.id)).getOrElse(ctx)
      U.RecordLiteral(
        selfRef.map(convertIdentifier(_)(using updatedCtx)),
        fields.map((fld, p) => (convertField(fld), convertPath(p)(using updatedCtx))),
        position
      )
    case A.UnitLiteral(position) =>
      U.UnitLiteral(position)
    case A.App(callee, arg, position) =>
      U.App(convertPath(callee), convertPath(arg), position)
    case A.Unbox(captureSet, boxed, position) =>
      U.Unbox(convertCaptureSetTree(captureSet), convertPath(boxed), position)
    case A.Let(varId, value, body, position) =>
      val newCtx = ctx.withNewId(varId.id)
      U.Let(
        convertIdentifier(varId)(using newCtx),
        convertTerm(value)(using ctx),
        convertTerm(body)(using newCtx),
        position
      )
    case A.Region(position) =>
      U.Region(position)
    case A.Deref(ref, position) =>
      U.Deref(convertPath(ref), position)
    case A.Assign(ref, newVal, position) =>
      U.Assign(convertPath(ref), convertPath(newVal), position)
    case A.Ref(regionCap, initVal, position) =>
      U.Ref(convertPath(regionCap), convertPath(initVal), position)
    case A.Modif(regionCap, fields, position) =>
      U.Modif(
        convertPath(regionCap),
        fields.map((fld, p) => (convertField(fld), convertPath(p))),
        position
      )
  }

  private def convertPath(path: A.Path)(using ctx: Ctx): U.Path = path match {
    case id: A.Identifier => convertIdentifier(id)
    case A.Select(root, select, position) => U.Select(convertPath(root), select, position)
  }

  private def convertIdentifier(ident: A.Identifier)(using ctx: Ctx): U.Identifier = {
    val A.Identifier(id, position) = ident
    U.Identifier(ctx.getCurrentIdFor(id, position), position)
  }

  private def convertField(fld: A.Field): U.Field = fld match {
    case namedField: A.NamedField => convertNamedField(namedField)
    case A.Reg(position) => U.Reg(position)
  }

  private def convertNamedField(fld: A.NamedField): U.NamedField = {
    val A.NamedField(fieldName, position) = fld
    U.NamedField(fieldName, position)
  }

  private def convertType(tpe: A.TypeTree)(using ctx: Ctx): U.TypeTree = {
    val A.TypeTree(shape, capt, position) = tpe
    U.TypeTree(convertShape(shape), capt.map(convertCaptureSetTree), position)
  }

  private def convertShape(shape: A.TypeShapeTree)(using ctx: Ctx): U.TypeShapeTree = shape match {
    case A.TopTypeTree(position) =>
      U.TopTypeTree(position)
    case A.AbsTypeTree(varId, varType, bodyType, position) =>
      val newCtx = ctx.withNewId(varId.id)
      U.AbsTypeTree(
        convertIdentifier(varId)(using newCtx),
        convertType(varType)(using ctx),
        convertType(bodyType)(using newCtx),
        position
      )
    case A.BoxTypeTree(boxedType, position) =>
      U.BoxTypeTree(convertType(boxedType), position)
    case A.UnitTypeTree(position) =>
      U.UnitTypeTree(position)
    case A.RefTypeTree(referencedType, position) =>
      U.RefTypeTree(convertShape(referencedType), position)
    case A.RegTypeTree(position) =>
      U.RegTypeTree(position)
    case A.RecordTypeTree(selfRef, fieldsInOrder, position) =>
      val updatedCtx = selfRef.map(selfRef => ctx.withNewId(selfRef.id)).getOrElse(ctx)
      U.RecordTypeTree(
        selfRef.map(convertIdentifier(_)(using updatedCtx)),
        fieldsInOrder.map((fld, tpe) => (convertNamedField(fld), convertType(tpe)(using updatedCtx))),
        position
      )
  }

  private def convertCaptureSetTree(capt: A.CaptureSetTree)(using ctx: Ctx): U.CaptureSetTree = capt match {
    case A.ExplicitCaptureSetTree(capturedVarsInOrder, position) =>
      U.ExplicitCaptureSetTree(capturedVarsInOrder.map(convertPath), position)
    case A.ImplicitCaptureSetTree(position) =>
      U.ImplicitCaptureSetTree(position)
  }

  private case class Ctx(
                          bindingsInScope: Map[String, UniqueVarId],
                          renamingIndices: MutMap[String, Int],
                          reporter: Reporter
                        ){

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
