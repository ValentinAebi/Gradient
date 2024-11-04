package gradcc.phases.typechecking

import gradcc.lang.*

def unpackIfRecursive(tpe: Type, selfRef: ProperPath): Type = tpe match {
  case Type(RecordShape(Some(selfVarId), fields), capSet) =>
    val substMap = Map[ProperPath, ProperPath](VarPath(selfVarId) -> selfRef)
    Type(
      RecordShape(None, fields.map(
        (fld, tpe) => (fld, substituteType(tpe)(using substMap)))
      ),
      capSet
    )
  case _ => tpe
}
