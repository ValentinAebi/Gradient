package gradcc.phases.typechecking

import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*
import gradcc.reporting.{Position, Reporter}

private[typechecking] type Store = Map[UniqueVarId, Option[Type]]

private[typechecking] case class Ctx(
                                      store: Store,
                                      pathEquivalences: Seq[(ProperPath, ProperPath)],
                                      reporter: Reporter
                                    ) {

  // TODO optimize (more replications of this object than needed)

  def withNewBinding(varId: UniqueVarId, varType: Option[Type]): Ctx =
    copy(store = store + (varId -> varType))

  def withNewPathEquivalence(p: ProperPath, q: ProperPath): Ctx =
    copy(pathEquivalences = pathEquivalences :+ (p, q))

  def withNewSelectEquivalence(owner: ProperPath, fld: RecordField, value: ProperPath): Ctx =
    withNewPathEquivalence(SelectPath(owner, fld), value)

  def varLookup(varId: VarId): Option[Type] = store.get(varId).flatten

  def pathLookup(capabilityPath: ProperPath): Option[Type] = capabilityPath match {
    case VarPath(root) => varLookup(root)
    case SelectPath(lhs, select) =>
      pathLookup(lhs)
        .map(unpackIfRecursive(_, lhs))
        .flatMap {
          case Type(RecordShape(selfRef, fields), _) =>
            fields.get(select)
          case _ => None
        }
  }

  def expressAsPathFrom(origin: ProperPath, target: ProperPath): Option[ProperPath] = {
    createPathsEquivalenceComputer().expressAsPathFrom(origin, target)
  }

  def equivalenceClassOf(id: UniqueVarId): Set[UniqueVarId] = {
    createPathsEquivalenceComputer().getEquivClass(id)
  }

  def reportError(msg: String, pos: Position): None.type = {
    reporter.error(msg, pos)
    None
  }

  private def createPathsEquivalenceComputer(): PathsEquivalenceComputer = {
    val pec = PathsEquivalenceComputer.empty
    for ((p, q) <- pathEquivalences) {
      pec.assertEquivalent(p, q)
    }
    pec
  }
  
}
