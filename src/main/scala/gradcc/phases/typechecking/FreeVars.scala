package gradcc.phases.typechecking

import gradcc.asts.UniqueVarId
import gradcc.lang.*

extension (v: UniqueVarId) {

  def isFreeIn(tpe: Type): Boolean = {
    val Type(shape, captureSet) = tpe
    isFreeIn(shape) || captureSet.exists(v.isFreeIn)
  }

  def isFreeIn(shape: Shape): Boolean = shape match {
    case TopShape => false
    case AbsShape(varId, varType, resType) => varId != v && (isFreeIn(varType) || isFreeIn(resType))
    case BoxShape(boxed) => isFreeIn(boxed)
    case UnitShape => false
    case RefShape(referenced) => isFreeIn(referenced)
    case RegionShape => false
    case RecordShape(selfRef, fields) => !selfRef.contains(v) && fields.exists((_, tpe) => isFreeIn(tpe))
  }

  def isFreeIn(capturable: Capturable): Boolean = capturable match {
    case VarPath(variable) => v == variable
    case SelectPath(lhs, select) => isFreeIn(lhs)
    case RootCapability => false
  }

}
