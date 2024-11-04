package gradcc.phases.typechecking

import gradcc.lang.*

type SubstMap = Map[ProperPath, ProperPath]

def substituteType(tpe: Type)(using SubstMap): Type = {
  val Type(shape, captureSet) = tpe
  Type(substituteShape(shape), substituteCaptureDescr(captureSet))
}

def substituteShape(shape: Shape)(using SubstMap): Shape = shape match {
  case TopShape =>
    TopShape
  case AbsShape(varId, varType, resType) =>
    AbsShape(varId, substituteType(varType), substituteType(resType))
  case BoxShape(boxed) =>
    BoxShape(substituteType(boxed))
  case UnitShape =>
    UnitShape
  case RefShape(referenced) =>
    RefShape(substituteShape(referenced))
  case RegionShape =>
    RegionShape
  case recordShape: RecordShape =>
    substituteRecordShape(recordShape)
}

def substituteRecordShape(recordShape: RecordShape)(using SubstMap): RecordShape = {
  val RecordShape(selfRef, fields) = recordShape
  RecordShape(selfRef, fields.map((fld, tpe) => (fld, substituteType(tpe))))
}

def substituteProperPath(p: ProperPath)(using subst: SubstMap): ProperPath = {
  subst.getOrElse(p, {
    p match {
      case capVar: VarPath => capVar
      case SelectPath(lhs, select) =>
        SelectPath(substituteProperPath(lhs), select)
    }
  })
}

def substituteCaptureDescr(capDescr: CaptureDescriptor)(using SubstMap): CaptureDescriptor = capDescr match {
  case CaptureSet(captured) => CaptureSet(captured.map(substituteCapturable))
  case Brand => Brand
}

def substituteCapturable(capturable: Capturable)(using subst: SubstMap): Capturable = capturable match {
  case p: ProperPath => substituteProperPath(p)
  case RootCapability => RootCapability
}
