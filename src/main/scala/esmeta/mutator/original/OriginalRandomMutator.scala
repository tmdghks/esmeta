package esmeta.mutator.original

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.mutator.original.* 
import esmeta.mutator.original.OriginalUtil.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.synthesizer.Synthesizer 
import esmeta.synthesizer.original.*

class OriginalRandomMutator(using cfg: CFG)(
  val synthesizer: Synthesizer = OriginalRandomSynthesizer(cfg.grammar), // todo(@tmdghks): replace with original one
) extends OriginalMutator {
  import OriginalRandomMutator.*

  val names = List("RandomMutator")

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] =
    val k = targetAstCounter(ast)
    if (k == 0)
      List.fill(n)(ast)
    else
      c = (n - 1) / k + 1
    shuffle(Walker.walk(ast)).take(n).map((name, _))

  /* number of new candidates to make for each target */
  var c = 0

  /** internal walker */
  object Walker extends OriginalUtil.AdditiveListWalker {
    override def walk(ast: Syntactic): List[Syntactic] =
      val mutants = super.walk(ast)
      if isTarget(ast) then List.tabulate(c)(_ => synthesizer(ast)) ++ mutants
      else mutants

    override def walk(lex: Lexical): List[Lexical] = lex.name match {
      case "NumericLiteral" =>
        List("0", "1", "0n", "1n").map(n => Lexical(lex.name, n))
      case "BooleanLiteral" =>
        List("true", "false").map(b => Lexical(lex.name, b))
      case _ => List(lex)
    }
  }
}

object OriginalRandomMutator {
  // true if the given ast is target ast
  def isTarget = (ast: Ast) =>
    List(
      "AssignmentExpression",
      "PrimaryExpression",
      "Statement",
      "Declaration",
    )
      .contains(ast.name)

  // count the number of target sub-ast
  val targetAstCounter = new OriginalUtil.AstCounter(isTarget)

  // todo(@tmdghks): fix error regarding the following line
  /// default random mutator
  // val default = RandomMutator()
}
