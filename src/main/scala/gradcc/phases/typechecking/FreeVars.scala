package gradcc.phases.typechecking

import gradcc.asts.UniqueVarId
import gradcc.lang.*

extension (v: UniqueVarId) {

  def isFreeIn(tpe: Type): Boolean = {
    val Type(shape, captureSet) = tpe
    isFreeIn(shape) || isFreeIn(captureSet)
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
    case BrandedPath(p) => isFreeIn(p)
    case RootCapability => false
  }
  
  def isFreeIn(captureDescr: CaptureDescriptor): Boolean = captureDescr match {
    case CaptureSet(captured) => captured.exists(isFreeIn)
    case Brand => false
  }

}

def freeVars(tpe: Type): Set[UniqueVarId] = {
  val Type(shape, captureSet) = tpe
  freeVars(shape) ++ freeVars(captureSet)
}

def freeVars(shape: Shape): Set[UniqueVarId] = shape match {
  case TopShape => Set.empty
  case AbsShape(varId, varType, resType) =>
    freeVars(varType) ++ (freeVars(resType) - varId)
  case BoxShape(boxed) => freeVars(boxed)
  case UnitShape => Set.empty
  case RefShape(referenced) => freeVars(referenced)
  case RegionShape => Set.empty
  case RecordShape(selfRef, fields) => fields.flatMap((_, tpe) => freeVars(tpe)).toSet -- selfRef
}

def freeVars(captureDescr: CaptureDescriptor): Set[UniqueVarId] = captureDescr match {
  case CaptureSet(captured) => captured.flatMap(freeVars)
  case Brand => Set.empty
}

def freeVars(capturable: Capturable): Set[UniqueVarId] = capturable match {
  case VarPath(root) => Set(root)
  case SelectPath(lhs, field) => freeVars(lhs)
  case BrandedPath(p) => freeVars(p)
  case RootCapability => Set.empty
}
