package gradcc.typechecking

import gradcc.asts.UniqueVarId
import gradcc.lang.{AbsShape, BoxShape, CapabilityPath, Capturable, RecordShape, RefShape, RegionShape, RootCapability, ShapeType, TopShape, Type, UnitShape}

extension (v: UniqueVarId) {

  def isFreeIn(tpe: Type): Boolean = {
    val Type(shape, captureSet) = tpe
    isFreeIn(shape) || captureSet.exists(isFreeIn)
  }

  def isFreeIn(shape: ShapeType): Boolean = shape match {
    case TopShape => false
    case AbsShape(varId, varType, resType) => varId != v && (isFreeIn(varType) || isFreeIn(resType))
    case BoxShape(boxed) => isFreeIn(boxed)
    case UnitShape => false
    case RefShape(referenced) => isFreeIn(referenced)
    case RegionShape => false
    case RecordShape(selfRef, fields) => !selfRef.contains(v) && fields.exists((_, tpe) => isFreeIn(tpe))
  }
  
  def isFreeIn(capturable: Capturable): Boolean = capturable match {
    case CapabilityPath(root, selects) => root == v
    case RootCapability => false
  }

}