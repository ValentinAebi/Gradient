package gradcc.typechecking

import gradcc.asts.UniqueVarId
import gradcc.asts.UniquelyNamedTerms.*
import gradcc.lang.*

def substitute(tpe: Type)(using subst: Map[UniqueVarId, Path]): Type = {
  val Type(shape, captureSet) = tpe
  Type(substitute(shape), captureSet.map(substitute))
}

def substitute(shape: ShapeType)(using subst: Map[UniqueVarId, Path]): ShapeType = shape match {
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

def substitute(capturable: Capturable)(using subst: Map[UniqueVarId, Path]): Capturable = capturable match {
  case CapabilityPath(root, selects) => ???
  case RootCapability => ???
}
