package esmeta.es

import esmeta.*
import esmeta.error.*
import esmeta.util.SystemUtils.readFile
import scala.util.Try
import sys.process._
import java.util.StringJoiner

/** JavaScript Transpiler utilities */
object JSTrans {

  def transpile(src: String): String = ???

  /** indicating the result of transpilation was faillure */
  val failTag = "TRANSPILE_FAILURE"

}
