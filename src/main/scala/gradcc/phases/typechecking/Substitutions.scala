package gradcc.phases.typechecking

import gradcc.lang.*

type SubstMap = Map[StablePath, StablePath]

def substitute(tpe: Type)(using SubstMap): Type = {
  val Type(shape, captureSet) = tpe
  Type(substitute(shape), substitute(captureSet))
}

def substitute(shape: Shape)(using SubstMap): Shape = shape match {
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

def substitute(recordShape: RecordShape)(using SubstMap): RecordShape = {
  val RecordShape(selfRef, fields) = recordShape
  RecordShape(selfRef, fields.map((fld, tpe) => (fld, substitute(tpe))))
}

def substitute(p: ProperPath)(using subst: SubstMap): StablePath = {
  subst.getOrElse(p, {
    p match {
      case capVar: VarPath => capVar
      case SelectPath(lhs, select) =>
        substitute(lhs) match {
          case p: ProperPath => SelectPath(p, select)
          case BrandedPath(p) => BrandedPath(SelectPath(p, select))
        }
    }
  })
}

def substitute(capDescr: CaptureDescriptor)(using SubstMap): CaptureDescriptor = capDescr match {
  case CaptureSet(captured) => CaptureSet(captured.map(substitute))
  case Brand => Brand
}

def substitute(capturable: Capturable)(using subst: SubstMap): Capturable = capturable match {
  case p: ProperPath =>
    subst.getOrElse(p, p)
  case BrandedPath(p) =>
    substitute(p) match {
      // TODO check that the resulting path must indeed always be branded
      case properPath: ProperPath => BrandedPath(properPath)
      case brandedPath: BrandedPath => brandedPath
    }
  case RootCapability => RootCapability
}
