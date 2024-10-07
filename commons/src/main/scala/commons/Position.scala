package commons

case class Position(file: String, line: Int, column: Int, lineContents: String){

  override def toString: String = s"$file:$line:$column"
  
}
