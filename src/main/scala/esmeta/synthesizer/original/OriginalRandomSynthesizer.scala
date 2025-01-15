package esmeta.synthesizer.original

import esmeta.synthesizer.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.spec.util.GrammarGraph
import esmeta.util.BaseUtils.*

/** A random ECMAScript AST synthesizer */
class OriginalRandomSynthesizer(
  val grammar: Grammar,
) extends Synthesizer {
  import grammar.*

  val graph = GrammarGraph(grammar)
  import graph.*

  /** synthesizer name */
  def name: String = "RandomSynthesizer"

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic =
    val synNode = getSyn(name, args)
    val arr = synEdges(synNode).toArray.map(r => r -> weight(r))
    if (arr.toList.exists { case (_, k) => k == 0 })
      println((synNode, arr.toList))
    val rhsNode = weightedChoose(arr)
    val rhs = rhsNode.rhs
    val rhsIdx = rhsNode.idx
    val isSingle = rhs.symbols.length == 1
    val children =
      rhs.symbols.map(synSymbol(rhsNode.argMap, !isSingle)).toVector
    Syntactic(name, args, rhsIdx, children)

  /** for lexical production */
  def apply(name: String): Lexical = origSimpleSyn(name)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  lazy val weight: Map[Node, Int] =
    fixpoint(Map(), topological, aux)(using Ordering[Int].reverse)

  private def aux(
    map: Map[Node, Int],
    node: Node,
  ): Int = node match
    case synNode: SynNode =>
      (for {
        rhsNode <- synEdges.getOrElse(synNode, Set())
      } yield map.getOrElse(rhsNode, 0)).sum
    case lexNode: LexNode => 1
    case rhsNode: RhsNode =>
      rhsNode.rhs.symbols match
        case List(nt: Nonterminal) =>
          map.getOrElse(getProd(nt, rhsNode.argMap), 0)
        case _ => 1

  private val origSimpleSyn = OriginalSimpleSynthesizer(grammar)

  private def synSymbol(argsMap: Map[String, Boolean], mayUseSimple: Boolean)(
    symbol: Symbol,
  ): Option[Ast] = symbol match
    case ButNot(nt, _) => synSymbol(argsMap, mayUseSimple)(nt)
    case Optional(symbol) =>
      if (randBool) None else synSymbol(argsMap, mayUseSimple)(symbol)
    case Nonterminal(name, args) =>
      if (origSimpleSyn.reservedLexicals contains name)
        Some(Lexical(name, origSimpleSyn.reservedLexicals(name)))
      else {
        import NonterminalArgumentKind.*
        val newArgs = for (arg <- args) yield arg.kind match
          case True  => true
          case False => false
          case Pass  => argsMap(arg.name)
        val syn =
          if (randBool && mayUseSimple) origSimpleSyn(name, newArgs)
          else apply(name, newArgs)
        Some(syn)
      }
    case _ => None
}
object RandomSynthesizer extends Synthesizer.Builder {
  def apply(grammar: Grammar) = new RandomSynthesizer(grammar)
}
