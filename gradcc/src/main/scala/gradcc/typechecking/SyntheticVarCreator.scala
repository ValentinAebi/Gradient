package gradcc.typechecking

import gradcc.asts.UniqueVarId

final class SyntheticVarCreator {
  private var nextIdx = 0
  
  def nextVar(): UniqueVarId = {
    val v = UniqueVarId("<auto>", nextIdx)
    nextIdx += 1
    v
  }

}
