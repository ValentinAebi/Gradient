package gradcc.prettyprinting

import gradcc.asts.TermsProvider
import gradcc.{Reporter, SimplePhase}

def PrettyprinterPhase(p: TermsProvider): SimplePhase[p.Term, String] = new SimplePhase[p.Term, String]("Prettyprinter") {

  override protected def runImpl(in: p.Term, reporter: Reporter): String = TermsPrettyprinter(p)(in)

}
