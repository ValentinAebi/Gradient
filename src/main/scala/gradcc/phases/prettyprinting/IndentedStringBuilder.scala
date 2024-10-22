package gradcc.phases.prettyprinting

import gradcc.lang.{Keyword, Operator}

final class IndentedStringBuilder(indent: String = "  ") {
  private val sb: StringBuilder = new StringBuilder()
  private var indentLevel: Int = 0

  def newLine(): IndentedStringBuilder = {
    sb.append("\n").append(indent.repeat(indentLevel))
    this
  }

  def incIndent(): IndentedStringBuilder = {
    indentLevel += 1
    this
  }

  def decIndent(): IndentedStringBuilder = {
    indentLevel -= 1
    this
  }

  def add(s: String): IndentedStringBuilder = {
    var isFirstLine = true
    allLines(s).foreach { line =>
      if (!isFirstLine){
        newLine()
      }
      isFirstLine = false
      sb.append(line)
    }
    this
  }

  def add(kw: Keyword): IndentedStringBuilder = {
    add(kw.str)
    this
  }

  def add(op: Operator): IndentedStringBuilder = {
    add(op.str)
    this
  }

  override def toString: String = sb.toString()

}

private def allLines(s: String): Seq[String] = {
  val lines = Seq.newBuilder[String]
  s.lines().forEach(lines.addOne)
  if (s.endsWith("\n") || s.endsWith("\r")){
    lines.addOne("")
  }
  lines.result()
}
