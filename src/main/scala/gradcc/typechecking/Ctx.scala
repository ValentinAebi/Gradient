package gradcc.typechecking

import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*
import gradcc.{Position, Reporter}

private[typechecking] type Store = Map[UniqueVarId, Option[Type]]

private[typechecking] case class Ctx(store: Store, reporter: Reporter) {

  def withNewBinding(varId: UniqueVarId, varType: Option[Type]): Ctx = copy(store = store + (varId -> varType))
  
  def varLookup(varId: VarId): Option[Type] = store.apply(varId)

  def pathLookup(capabilityPath: Path): Option[Type] = capabilityPath match {
    case VarPath(root) => store.apply(root)
    case SelectPath(lhs, select) =>
      pathLookup(lhs).flatMap {
        case Type(RecordShape(selfRef, fields), _) =>
          fields.get(select).map { fieldType =>
            selfRef.map(selfRef => substitute(fieldType)(using Map(VarPath(selfRef) -> lhs))).getOrElse(fieldType)
          }
        case _ => None
      }
  }

  def reportError(msg: String, pos: Position): None.type = {
    reporter.error(msg, pos)
    None
  }
}
