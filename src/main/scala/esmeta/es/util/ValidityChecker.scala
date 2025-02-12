package esmeta.es.util

import scala.util.Try
import esmeta.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.es.JSEngine
import esmeta.util.BaseUtils.warn

/** ECMAScript program validity checker */
object ValidityChecker {
  def apply(grammar: Grammar, ast: Ast): Boolean =
    apply(ast.toString(grammar = Some(grammar)))
  def apply(code: String): Boolean =
    JSEngine.default.fold(true)(_.isValid(code))
}
