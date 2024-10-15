package gradcc.typechecking

import gradcc.asts.UniqueVarId

final class SyntheticVarCreator {
  private var nextIdx = 0
  
  def nextVar(name: String): UniqueVarId = {
    val v = UniqueVarId(name, nextIdx)
    nextIdx += 1
    v
  }

}
