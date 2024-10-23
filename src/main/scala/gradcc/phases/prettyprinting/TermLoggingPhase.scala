package gradcc.phases.prettyprinting

import gradcc.asts.TermsProvider
import gradcc.phases.{Phase, SimplePhase}
import gradcc.reporting.Reporter

import java.io.PrintStream

def TermLoggingPhase(p: TermsProvider, ps: PrintStream): Phase[p.R[p.TermTree], p.R[p.TermTree]] =
  new SimplePhase[p.R[p.TermTree], p.R[p.TermTree]]("TermLogger") {

    override val acceptsFaultyInput: Boolean = true

    override protected def runImpl(in: p.R[p.TermTree], reporter: Reporter): p.R[p.TermTree] = {
      val str = TermsPrettyprinter(p)(in)
      ps.println(str)
      in
    }

  }
