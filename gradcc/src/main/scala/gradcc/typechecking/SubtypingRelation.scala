package gradcc.typechecking

import gradcc.lang.*

object SubtypingRelation {

  extension (l: Type) def subtypeOf(r: Type)(using Ctx): Boolean = {
    val Type(lSh, lCap) = l
    val Type(rSh, rCap) = r
    lSh.subshapeOf(rSh) && lCap.subcaptureOf(rCap)
  }

  extension (l: Set[Capturable]) def subcaptureOf(r: Set[Capturable])(using ctx: Ctx): Boolean = {
    if (l.size == 1) {
      l.head match {
        // Sc-path
        case p: CapabilityPath if ctx.pathLookup(p).exists(_.captureSet.subcaptureOf(r)) => true
        // Sc-elem
        case p if r.contains(p) => true
        // Sc-mem
        case CapPath(lhs, select) if Set(lhs).subcaptureOf(r) => true
        case _ => false
      }
    } else {
      // Sc-set
      l.forall(c => Set(c).subcaptureOf(r))
    }
  }

  extension (l: ShapeType) def subshapeOf(r: ShapeType)(using Ctx): Boolean = (l, r) match {
    // Top
    case (_, TopShape) => true
    // Refl
    case (l, r) if l == r => true
    // Fun
    case (AbsShape(_, lVarT, lResT), AbsShape(_, rVarT, rResT)) =>
      rVarT.subtypeOf(lVarT) && lResT.subtypeOf(rResT)
    // Boxed
    case (BoxShape(bl), BoxShape(br)) => bl.subtypeOf(br)
    // Rec
    case (RecordShape(lSelfRef, lFields), RecordShape(rSelfRef, rFields)) =>
      lFields.forall((lfn, lft) => rFields.get(lfn).exists(lft.subtypeOf))
    case _ => false
  }

}
