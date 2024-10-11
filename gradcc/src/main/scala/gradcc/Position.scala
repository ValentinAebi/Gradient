package gradcc

import scala.util.parsing.input

case class Position(file: String, line: Int, column: Int, lineContents: String) extends input.Position {

  override def toString: String = s"$file:$line:$column"
  
}
