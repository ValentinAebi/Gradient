package gradcc.phases.prettyprinting

import gradcc.asts.TermsProvider
import gradcc.phases.SimplePhase
import gradcc.reporting.Reporter

def PrettyprinterPhase(p: TermsProvider, forceIgnoreTypes: Boolean = false): SimplePhase[p.R[p.TermTree], String] =
  new SimplePhase[p.R[p.TermTree], String]("Prettyprinter") {

    override val acceptsFaultyInput: Boolean = true

    override protected def runImpl(in: p.R[p.TermTree], reporter: Reporter): String =
      TermsPrettyprinter(p, forceIgnoreTypes)(in)

  }
