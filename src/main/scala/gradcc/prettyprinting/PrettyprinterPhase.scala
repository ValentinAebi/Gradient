package gradcc.prettyprinting

import gradcc.asts.TermsProvider
import gradcc.{Reporter, SimplePhase}

def PrettyprinterPhase(p: TermsProvider, forceIgnoreTypes: Boolean = false): SimplePhase[p.R[p.Term], String] =
  new SimplePhase[p.R[p.Term], String]("Prettyprinter") {

    override val acceptsFaultyInput: Boolean = true

    override protected def runImpl(in: p.R[p.Term], reporter: Reporter): String =
      TermsPrettyprinter(p, forceIgnoreTypes)(in)

  }
