package commons

final case class Position(file: String, line: Int, col: Int){

  override def toString: String = s"$file:$line:$col"
  
}
