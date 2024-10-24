package gradcc.phases.typechecking

import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*

def substitute(tpe: Type)(using subst: Map[Capturable, Path]): Type = {
  val Type(shape, captureSet) = tpe
  Type(substitute(shape), captureSet.map(substitute))
}

def substitute(shape: Shape)(using subst: Map[Capturable, Path]): Shape = shape match {
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

def substitute(recordShape: RecordShape)(using subst: Map[Capturable, Path]): RecordShape = {
  val RecordShape(selfRef, fields) = recordShape
  RecordShape(selfRef, fields.map((fld, tpe) => (fld, substitute(tpe))))
}

def substitute(capturable: Capturable)(using subst: Map[Capturable, Path]): Capturable = {
  subst.getOrElse(capturable, capturable match {
    case path: Path => substitute(path)
    case RootCapability => RootCapability
  })
}

def substitute(capabilityPath: Path)(using subst: Map[Capturable, Path]): Path = {
  subst.getOrElse(capabilityPath, {
    capabilityPath match {
      case capVar: VarPath => capVar
      case SelectPath(lhs, select) => SelectPath(substitute(lhs), select)
    }
  })
}
