package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*

trait CommandDecl { self: Self =>

  /** commands */
  abstract class Command(
    /** command name */
    val name: String,
    /** command help message */
    val help: String = "",
  ) {

    /** options */
    val options: List[String]

    /** run command */
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit

    /** not yet supported message */
    def notYetCmd: Unit =
      notYet("this command is not yet supported")
    def notYet(msg: String): Unit =
      println(s"[NotSupported] $msg @ $name")
  }
  object Command {
    val commands: List[Command] = List(
      CmdHelp,
      CmdContinue,
      CmdMove,
      CmdBreak,
      CmdListBreak,
      CmdRmBreak,
      CmdJump,
      CmdPrint,
      CmdLog,
      CmdGraph,
      CmdExit,
      CmdStop,
      CmdInfo,
      CmdEntry,
      CmdWorklist,
      CmdFindImprec,
      CmdFindMerged,
    )
    val cmdMap: Map[String, Command] =
      commands.map(cmd => (cmd.name, cmd)).toMap
  }
}
