package gradcc.phases.typechecking

import gradcc.lang.{AbsShape, BoxShape, Brand, BrandedPath, Capturable, CaptureDescriptor, CaptureSet, ProperPath, RecordShape, RefShape, RegionShape, RootCapability, Shape, TopShape, Type, UnitShape}

def freePaths(tpe: Type): Set[ProperPath] = {
  val Type(shape, cs) = tpe
  freePaths(shape) ++ freePaths(cs)
}

def freePaths(shape: Shape): Set[ProperPath] = shape match {
  case TopShape =>
    Set.empty
  case AbsShape(varId, varType, resType) =>
    freePaths(varType) ++ freePaths(resType).filterNot(_.isRootedIn(varId))
  case BoxShape(boxed) =>
    freePaths(boxed)
  case UnitShape =>
    Set.empty
  case RefShape(referenced) =>
    freePaths(referenced)
  case RegionShape =>
    Set.empty
  case RecordShape(selfRef, fields) =>
    fields.flatMap((_, tpe) => freePaths(tpe))
      .filterNot(p => selfRef.exists(s => p.isRootedIn(s)))
      .toSet
}

def freePaths(captureDescriptor: CaptureDescriptor): Set[ProperPath] = captureDescriptor match {
  case CaptureSet(captured) => captured.flatMap(freePaths)
  case Brand => Set.empty
}

def freePaths(capturable: Capturable): Set[ProperPath] = capturable match {
  case p: ProperPath => Set(p)
  case BrandedPath(p) => Set(p)
  case RootCapability => Set.empty
}
