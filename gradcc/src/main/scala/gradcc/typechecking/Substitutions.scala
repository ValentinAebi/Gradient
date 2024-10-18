package gradcc.typechecking

import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*

def substitute(tpe: Type)(using subst: Map[Capturable, CapabilityPath]): Type = {
  val Type(shape, captureSet) = tpe
  Type(substitute(shape), captureSet.map(substitute))
}

def substitute(shape: ShapeType)(using subst: Map[Capturable, CapabilityPath]): ShapeType = shape match {
  case TopShape =>
    TopShape
  case AbsShape(varId, varType, resType) =>
    AbsShape(varId, substitute(varType), substitute(resType))
  case BoxShape(boxed) =>
    BoxShape(substitute(boxed))
  case UnitShape =>
    UnitShape
  case RefShape(referenced) =>
    RefShape(substitute(referenced))
  case RegionShape =>
    RegionShape
  case RecordShape(selfRef, fields) =>
    RecordShape(selfRef, fields.map((fld, tpe) => (fld, substitute(tpe))))
}

def substitute(capturable: Capturable)(using subst: Map[Capturable, CapabilityPath]): Capturable = {
  subst.getOrElse(capturable, capturable match {
    case path: CapabilityPath => substitute(path)
    case RootCapability => RootCapability
  })
}

def substitute(capabilityPath: CapabilityPath)(using subst: Map[Capturable, CapabilityPath]): CapabilityPath = {
  subst.getOrElse(capabilityPath, {
    capabilityPath match {
      case capVar: CapVar => capVar
      case CapPath(lhs, select) => CapPath(substitute(lhs), select)
    }
  })
}
