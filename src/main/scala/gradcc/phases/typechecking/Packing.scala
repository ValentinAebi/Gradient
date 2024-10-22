package gradcc.phases.typechecking

import gradcc.lang.*

private def unpackIfRecursive(tpe: Type, selfRef: Path): Type = tpe match {
  case Type(RecordShape(Some(selfVarId), fields), capSet) =>
    val substMap = Map[Capturable, Path](VarPath(selfVarId) -> selfRef)
    Type(
      RecordShape(None, fields.map(
        (fld, tpe) => (fld, substitute(tpe)(using substMap)))
      ),
      capSet.map(
        substitute(_)(using substMap)
      )
    )
  case _ => tpe
}
