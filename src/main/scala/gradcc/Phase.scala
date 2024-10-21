package gradcc

import gradcc.Reporter.FatalErrorException

sealed trait Phase[-In, +Out] {
  topLevelPhase =>

  val phaseName: String
  val acceptsFaultyInput: Boolean

  def run(in: In, reporter: Reporter): PhaseResult[Out]

  final infix def andThen[NextOut](next: Phase[Out, NextOut]): Phase[In, NextOut] =
    ComposedPhase[In, Out, NextOut](this, next)

}

trait SimplePhase[-In, +Out](override val phaseName: String) extends Phase[In, Out] {

  override final def run(in: In, reporter: Reporter): PhaseResult[Out] = {
    try {
      val res = runImpl(in, reporter)
      if reporter.errorFlagIsRaised then NonFatal(res) else Success(res)
    } catch {
      case fatalErrorException: FatalErrorException =>
        Fatal(fatalErrorException)
    }
  }

  protected def runImpl(in: In, reporter: Reporter): Out

}

final class ComposedPhase[In, Mid, Out](firstPhase: Phase[In, Mid], secondPhase: Phase[Mid, Out])
  extends Phase[In, Out] {

  override val phaseName: String = s"${firstPhase.phaseName} => ${secondPhase.phaseName}"

  override val acceptsFaultyInput: Boolean = firstPhase.acceptsFaultyInput

  override def run(in: In, reporter: Reporter): PhaseResult[Out] = {
    firstPhase.run(in, reporter) match
      case Success(mid) => secondPhase.run(mid, reporter)
      case NonFatal(faultyMid) if secondPhase.acceptsFaultyInput => secondPhase.run(faultyMid, reporter)
      case NonFatal(faultyMid) => reporter.impossibleToRunPhaseDueToErrors(secondPhase)
      case fatal: Fatal => fatal
  }

}

final class MultiPhase[In, Out](unaryPhase: Phase[In, Out]) extends Phase[Seq[In], Seq[Out]] {
  override val phaseName: String = unaryPhase.phaseName

  override val acceptsFaultyInput: Boolean = unaryPhase.acceptsFaultyInput

  override def run(in: Seq[In], reporter: Reporter): PhaseResult[Seq[Out]] = {
    in.toList.foldRight[PhaseResult[List[Out]]](Success(Nil)) {
      case (_, fatal: Fatal) => fatal
      case (unaryIn, accRes) => {
        val unaryOut = unaryPhase.run(unaryIn, reporter)
        (unaryOut, accRes) match {
          case (Success(out), Success(outs)) => Success(out :: outs)
          case (Success(out), NonFatal(outs)) => NonFatal(out :: outs)
          case (NonFatal(out), Success(outs)) => NonFatal(out :: outs)
          case (NonFatal(out), NonFatal(outs)) => NonFatal(out :: outs)
          case (fatal: Fatal, _) => fatal
          case (_, fatal: Fatal) => fatal
        }
      }
    }
  }
}

sealed trait PhaseResult[+T] {
  def isSuccess: Boolean = isInstanceOf[Success[?]]

  def resultOrThrow(): T
}

final case class Success[T](value: T) extends PhaseResult[T] {
  override def resultOrThrow(): T = value
}

final case class NonFatal[T](faultyValue: T) extends PhaseResult[T] {
  override def resultOrThrow(): T = faultyValue
}

final case class Fatal(fatalErrorException: FatalErrorException) extends PhaseResult[Nothing] {
  override def resultOrThrow(): Nothing = throw new NoSuchElementException()
}
