package gradcc.typechecking

import gradcc.asts.UniqueVarId
import gradcc.lang.Keyword
import gradcc.parsing.ScannerPhase

import scala.collection.mutable

final class SyntheticVarCreator {
  private val nextIdxPerName: mutable.Map[String, Int] = mutable.Map.empty
  
  def nextVar(name: String): UniqueVarId = {
    require(!ScannerPhase.lowerWordRegex.matches(name) || Keyword.values.exists(_.str == name),
      "synthetic variable names should be unparsable, otherwise there is a risk of variable clash with user-defined variables")
    val nextIdx = nextIdxPerName.getOrElse(name, 0)
    val v = UniqueVarId(name, nextIdx)
    nextIdxPerName(name) = nextIdx + 1
    v
  }

}
