package gradcc.lang

sealed trait RecordField

case class NamedField(id: String) extends RecordField {
  override def toString: String = id
}

case object RegionField extends RecordField {
  override def toString: String = Keyword.RegKw.str
}
