package gradcc


case class Position(file: String, line: Int, column: Int, lineContents: String) {

  override def toString: String = s"$file:$line:$column"
  
  def lineDisplay: String = lineContents + "\n" + " ".repeat(column-1) + "^"
  
}
