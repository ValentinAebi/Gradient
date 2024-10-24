package gradcc.phases.typechecking

import gradcc.lang.*

def unpackIfRecursive(tpe: Type, selfRef: Path): Type = tpe match {
  case Type(RecordShape(Some(selfVarId), fields), capSet) =>
    val substMap = Map[Capturable, Path](VarPath(selfVarId) -> selfRef)
    Type(
      RecordShape(None, fields.map(
        (fld, tpe) => (fld, substitute(tpe)(using substMap)))
      ),
      capSet
    )
  case _ => tpe
}
