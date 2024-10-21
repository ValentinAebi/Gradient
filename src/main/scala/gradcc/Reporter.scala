package gradcc

import gradcc.Reporter.{Entry, FatalErrorException}

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.collection.mutable.ListBuffer


final class Reporter {

  private val errors: ListBuffer[Entry] = ListBuffer.empty
  private val warnings: ListBuffer[Entry] = ListBuffer.empty
  private val infos: ListBuffer[Entry] = ListBuffer.empty

  private var errorFlag: Boolean = false

  def errorFlagIsRaised: Boolean = errorFlag

  def somethingReported: Boolean = errorFlagIsRaised || warnings.nonEmpty || infos.nonEmpty

  def impossibleToRunPhaseDueToErrors(phase: Phase[?, ?]): Fatal =
    Fatal(FatalErrorException(Entry(s"cannot run phase ${phase.phaseName} due to previous errors", None)))

  def fatal(msg: String, pos: Option[Position]): Nothing = {
    errorFlag = true
    val entry = Entry(msg, pos)
    errors.addOne(entry)
    throw FatalErrorException(entry)
  }

  def fatal(msg: String, pos: Position): Nothing = {
    fatal(msg, Some(pos))
  }

  def error(msg: String, pos: Option[Position]): Unit = {
    errorFlag = true
    errors.addOne(Entry(msg, pos))
  }

  def error(msg: String, pos: Position): Unit = {
    error(msg, Some(pos))
  }

  def warning(msg: String, pos: Option[Position]): Unit = {
    warnings.addOne(Entry(msg, pos))
  }

  def warning(msg: String, pos: Position): Unit = {
    warning(msg, Some(pos))
  }

  def info(msg: String, pos: Option[Position]): Unit = {
    infos.addOne(Entry(msg, pos))
  }

  def info(msg: String, pos: Position): Unit = {
    info(msg, Some(pos))
  }

  def dump(ps: PrintStream): Unit = {
    dumpList(ps, errors, "error(s)")
    dumpList(ps, warnings, "warning(s)")
    dumpList(ps, infos, "info(s)")
  }

  def getStringReport: String = {
    val bytes = new ByteArrayOutputStream()
    val ps = new PrintStream(bytes)
    dump(ps)
    bytes.toString
  }

  private def dumpList(ps: PrintStream, ls: ListBuffer[Entry], reportType: String): Unit = {
    ps.println(s"----------------- ${ls.size} $reportType ".padTo(60, '-'))
    for (entry <- ls) {
      ps.println(entry)
    }
  }

}

object Reporter {

  private[Reporter] final case class Entry(msg: String, pos: Option[Position]) {
    override def toString: String =
      s"${pos.map(pos => s"[$pos] ").getOrElse("")}$msg\n" + pos.map(_.lineDisplay).getOrElse("")
  }

  final case class FatalErrorException private[Reporter](private val entry: Entry) extends Exception {
    export entry.{msg, pos}

    override def toString: String = entry.toString
  }

}

