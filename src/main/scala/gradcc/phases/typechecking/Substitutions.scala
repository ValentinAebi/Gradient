package gradcc.phases.typechecking

import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*

def substitute(tpe: Type)(using subst: Map[Capturable, ProperPath]): Type = {
  val Type(shape, captureSet) = tpe
  Type(substitute(shape), captureSet.map(substitute))
}

def substitute(shape: Shape)(using subst: Map[Capturable, ProperPath]): Shape = shape match {
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
  case recordShape: RecordShape =>
    substitute(recordShape)
}

def substitute(recordShape: RecordShape)(using subst: Map[Capturable, ProperPath]): RecordShape = {
  val RecordShape(selfRef, fields) = recordShape
  RecordShape(selfRef, fields.map((fld, tpe) => (fld, substitute(tpe))))
}

def substitute(capturable: Capturable)(using subst: Map[Capturable, ProperPath]): Capturable = {
  subst.getOrElse(capturable, capturable match {
    case path: ProperPath => substitute(path)
    case RootCapability => RootCapability
  })
}

def substitute(capabilityPath: ProperPath)(using subst: Map[Capturable, ProperPath]): ProperPath = {
  subst.getOrElse(capabilityPath, {
    capabilityPath match {
      case capVar: VarPath => capVar
      case SelectPath(lhs, select) => SelectPath(substitute(lhs), select)
    }
  })
}
