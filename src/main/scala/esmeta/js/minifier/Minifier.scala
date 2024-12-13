package esmeta.js.minifier

import java.util.StringJoiner
import scala.util.*
import scala.sys.process.*
import esmeta.LINE_SEP
import esmeta.error.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.TimeoutException

object Minifier {
  val minifyCmd = Map(
    "swc" -> "minify-runner -v swc@1.4.6",
    "checkDiffSwc" -> "minify-runner -v swc@1.4.6 -d",
    "checkDiffTerser" -> "minify-runner -v terser@5.7.2 -d",
    "checkDiffBabel" -> "minify-runner -v babel@0.5.0 -d",
  )

  lazy val useSwc: Boolean = minifySwc(";").isSuccess

  def execScript(
    command: String,
    src: String,
    timeout: Option[Int] = None,
  ): Try[String] = Try {
    val escapedSrc = escapeToShellString(src)
    val stdout = new StringJoiner(LINE_SEP)
    val stderr = new StringJoiner(LINE_SEP)
    def cmd(main: String) = timeout match
      case Some(timeout) => s"timeout ${timeout}s $main $escapedSrc"
      case None          => s"$main $escapedSrc"
    val pb: ProcessBuilder = if command.contains("|") then {
      val Array(main, envInfo) = command.split("\\|")
      val Array(envKey, envVal) = envInfo.split(":")
      Process(cmd(main), None, envKey -> envVal)
    } else cmd(command)

    pb ! ProcessLogger(
      out => stdout.add(out),
      err => stderr.add(err),
    ) match {
      case 0         => stdout.toString
      case 124 | 137 => throw TimeoutException(command)
      case 127       => throw NoCommandError(command)
      case st        => throw new Exception(stdout.toString + stderr.toString)
    }
  }

  def minifySwc(src: String): Try[String] = execScript(minifyCmd("swc"), src)

  def checkMinifyDiffSwc(code: String): Boolean =
    checkMinifyDiff(code, Some("swc"))

  def checkMinifyDiff(code: String, cmd: Option[String]): Boolean =
    val minifierCode = cmd match
      case Some("swc") | Some("Swc")       => "checkDiffSwc"
      case Some("terser") | Some("Terser") => "checkDiffTerser"
      case Some("babel") | Some("Babel")   => "checkDiffBabel"
      case None =>
        println("No minifier specified. Using SWC as default.")
        "checkDiffSwc"
      case _ => throw new Exception("Invalid minifier specified.")
    try {
      val result = execScript(minifyCmd(minifierCode), code)
      result match {
        case Success(minifiedAndDiff) =>
          val diffResult = minifiedAndDiff.split(LINE_SEP).last
          if diffResult == "true" then true
          else false
        case Failure(exception) =>
          // println(s"[minify-check] $code $exception")
          false
      }
    } catch {
      case err => false
    }
}
