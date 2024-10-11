package gradcc.typechecking

import gradcc.{Position, Reporter}
import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.Type

import scala.collection.mutable

private[typechecking] type TermsTypes = mutable.Map[Term, Option[Type]]
private[typechecking] type Store = Map[UniqueVarId, Option[Type]]

private[typechecking] case class Ctx(store: Store, types: TermsTypes, reporter: Reporter) {

  def withNewBinding(varId: UniqueVarId, varType: Option[Type]): Ctx = copy(store = store + (varId -> varType))
  
  def saveType(term: Term, tpe: Option[Type]): Unit = {
    types(term) = tpe
  }

  def reportError(msg: String, pos: Position): None.type = {
    reporter.error(msg, pos)
    None
  }
}
