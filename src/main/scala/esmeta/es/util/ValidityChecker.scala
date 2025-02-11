package esmeta.es.util

import scala.util.Try
import esmeta.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.es.JSEngine
import esmeta.util.BaseUtils.warn

/** ECMAScript program validity checker */
object ValidityChecker {
  val MESSAGE = "VALIDITY_CHECKER_EXPECTED_EXCEPTION"

  def apply(grammar: Grammar, ast: Ast): Boolean =
    apply(ast.toString(grammar = Some(grammar)))
  def apply(code: String): Boolean =
    val src = s"${USE_STRICT}throw \"$MESSAGE\";$LINE_SEP;$LINE_SEP$code"
    JSEngine.default.fold(true)(engine =>
      checkValid(engine.runWithTimeout(src, Some(1))),
    )

  private def checkValid(result: Try[Any]): Boolean =
    result.failed.filter(_.getMessage contains MESSAGE).isSuccess
}
