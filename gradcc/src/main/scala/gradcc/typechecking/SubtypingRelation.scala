package gradcc.typechecking

import gradcc.lang.*

object SubtypingRelation {

  extension (l: Type) def subtypeOf(r: Type)(using Ctx): Boolean = {
    val Type(lSh, lCap) = l
    val Type(rSh, rCap) = r
    lSh.subshapeOf(rSh) && lCap.subcaptureOf(rCap)
  }

  extension (l: Set[Capturable]) def subcaptureOf(r: Set[Capturable])(using Ctx): Boolean = {
    if r.contains(RootCapability) then true
    else if l.contains(RootCapability) then false
    else {
      val lPaths = l.map(_.asInstanceOf[CapabilityPath]).groupBy(_.root)
      val rPaths = r.map(_.asInstanceOf[CapabilityPath]).groupBy(_.root)
      ???
    }
  }

  private def coveredBy(capt: CapabilityPath, captSet: Set[CapabilityPath], store: Store)(using Ctx): Boolean = {
    val CapabilityPath(root, selects) = capt
    ???
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
