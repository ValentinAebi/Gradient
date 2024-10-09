package gradcc.typechecking

import gradcc.lang.*
import gradcc.asts.UniquelyNamedTerms.*

def substitutePathsInType(tpe: Type, subst: Map[String, Path]): Type = {
  val Type(shape, captSet) = tpe
  ???
}

def substitutePathsInShape(shapeType: ShapeType, subst: Map[String, Path]): ShapeType = shapeType match {
  case TopShape => TopShape
  case AbsShape(varId, varType, resType) => ???
  case BoxShape(boxed) => ???
  case UnitShape => ???
  case RefShape(referenced) => ???
  case RegionShape => ???
  case RecordShape(fields, selfRef) => ???
}
