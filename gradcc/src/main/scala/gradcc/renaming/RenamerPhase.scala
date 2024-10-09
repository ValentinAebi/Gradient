package gradcc.renaming

import commons.{Position, Reporter, SimplePhase}
import gradcc.asts.{UniqueVarId, AmbiguouslyNamedTerms as Amb, UniquelyNamedTerms as Un}

import scala.collection.mutable.Map as MutMap

final class RenamerPhase extends SimplePhase[Amb.Term, Un.Term]("Renamer") {
  private val unknownId: UniqueVarId = UniqueVarId("<unknown>", -1)

  override protected def runImpl(in: Amb.Term, reporter: Reporter): Un.Term =
    convert(in)(using Ctx(Map.empty, MutMap.empty, reporter))
  
  private def convert(term: Amb.Term)(using ctx: Ctx): Un.Term = term match {
    case Amb.Identifier(id, position) => Un.Identifier(ctx.getCurrentIdFor(id, position), position)
    case Amb.Cap(position) => Un.Cap(position)
    case Amb.Select(root, select, position) => ???
    case Amb.Box(boxed, position) => ???
    case Amb.Abs(varId, tpe, body, position) => ???
    case Amb.RecordLiteral(fields, selfRef, position) => ???
    case Amb.UnitLiteral(position) => ???
    case Amb.App(callee, arg, position) => ???
    case Amb.Unbox(captureSet, boxed, position) => ???
    case Amb.Let(varId, value, body, position) => ???
    case Amb.Region(position) => ???
    case Amb.Deref(ref, position) => ???
    case Amb.Assign(ref, newVal, position) => ???
    case Amb.Ref(regionCap, initVal, position) => ???
    case Amb.Modif(regionCap, fields, position) => ???
  }

  private case class Ctx(
                          bindingsInScope: Map[String, UniqueVarId],
                          renamingIndices: MutMap[String, Int],
                          reporter: Reporter
                        ){

    def withNewId(s: String): Ctx = {
      val idx = renamingIndices.getOrElse(s, 0)
      renamingIndices(s) = idx + 1
      val newId = UniqueVarId(s, idx)
      copy(bindingsInScope = bindingsInScope.updatedWith(s)(_ => Some(newId)))
    }

    def getCurrentIdFor(s: String, pos: Position): UniqueVarId = bindingsInScope.getOrElse(s, {
      reporter.error(s"not found: $s", pos)
      unknownId
    })

  }

}
