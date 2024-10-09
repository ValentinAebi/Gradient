package gradcc.typechecking

import commons.{Position, Reporter}
import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.Type

import scala.collection.mutable

private[typechecking] type TermsTypes = mutable.Map[Term, Option[Type]]
private[typechecking] type Store = Map[UniqueVarId, Type]

private[typechecking] case class Ctx(store: Store, types: TermsTypes, reporter: Reporter) {

  def withNewBinding(varId: UniqueVarId, varType: Type): Ctx = copy(store = store + (varId -> varType))

  def reportError(msg: String, pos: Position): None.type = {
    reporter.error(msg, pos)
    None
  }
}
