package esmeta.es.util.fuzzer

import scala.annotation.tailrec
import scala.collection.mutable.Map as MMap
import esmeta.util.BaseUtils.{computeChiSq, chiSqDistTable}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.util.SystemUtils.*

object FSTreeWrapper:
  def fromDir(baseDir: String, fixed: Boolean): FSTreeWrapper =
    given fsTreeConfigDecoder: Decoder[FSTreeConfig] = deriveDecoder
    val config = readJson[FSTreeConfig](f"$baseDir/fstree-config.json")
    val fsTrieWrapper = FSTreeWrapper(config, fixed = fixed)
    fsTrieWrapper.replaceRootFromFile(f"$baseDir/fstree-root.json")
    fsTrieWrapper

  def debug: Unit =
    val tree = FSTreeWrapper(
      FSTreeConfig(
        promotionThreshold = chiSqDistTable("0.01"),
        demotionThreshold = chiSqDistTable("0.05"),
      ),
    )
    import tree.given
    val hitStacks = List(
      List("a"),
      List("a"),
      List("a"),
      List("a"),
    )
    val missStacks = List(
      List("b"),
      List("c"),
      List("d"),
      List("e"),
      List("f"),
      List("g"),
      List("h"),
      List("i"),
      List("j"),
      List("k"),
      List("l"),
      List("m"),
      List("n"),
      List("o"),
      List("p"),
      List("q"),
      List("r"),
      List("s"),
    )
    tree.touchWithHit(hitStacks)
    tree.touchWithMiss(missStacks)
    println(tree(List("a")))
    println(tree(List("b")))
    println(tree.root)

class FSTreeWrapper(
  val config: FSTreeConfig,
  val debug: Boolean = false,
  var fixed: Boolean = false,
) {
  val fixedSensMap = MMap.empty[List[String], Int]
  var root: FSTree = FSTree(status = FSTreeStatus.Noticed, depth = 0)

  var sensDistr: MMap[Int, Int] =
    MMap.from(0 to config.maxSensitivity map (_ -> 0))

  private def computeFeatureChiSq(
    hits: Long,
    misses: Long,
    pHits: Long,
    pMisses: Long,
  ): Double = {
    // root is noticed without any conditions
    if (pHits == 0 || pMisses == 0) chiSqDistTable.values.max + 1
    else
      val absentHits = pHits - hits
      val absentMisses = pMisses - misses
      val chiSq = computeChiSq(hits, misses, absentHits, absentMisses)
      assert(
        chiSq >= 0,
        f"Score for rootHits: $pHits, rootMisses: $pMisses, hits: $hits, misses: $misses is negative: $chiSq",
      )
      assert(
        chiSq.isFinite,
        f"Score for rootHits: $pHits, rootMisses: $pMisses, hits: $hits, misses: $misses is not finite: $chiSq",
      )
      if (hits + misses < config.minTouch) 0 else chiSq
  }

  /** Insert feature stacks from a single script into the tree. The script
    * succeeded to invoke some non-trivial minifier operations. Increment the
    * hits of each node in the tree.
    *
    * @param stacks
    *   the feature stacks generated from the successful script
    */
  def touchWithHit(stacks: Iterable[List[String]]): Unit =
    if !fixed then
      stacks.foreach { s =>
        root.touchByStack(s.take(config.maxSensitivity), isHit = true)
      }
      root.writeback()
      root.updateStatus()

  /** Insert feature stacks from a single script into the tree. The script
    * failed to invoke some non-trivial minifier operations. Increment the
    * misses of each node in the tree.
    *
    * @param stacks
    *   the feature stacks generated from the failed script
    */
  def touchWithMiss(stacks: Iterable[List[String]]): Unit =
    if !fixed then
      stacks.foreach { s =>
        root.touchByStack(s.take(config.maxSensitivity), isHit = false)
      }
      root.writeback()
      root.updateStatus()

  def apply(stack: List[String]): Int =
    val tmpStack = stack.take(config.maxSensitivity)
    if fixed then fixedSensMap.getOrElseUpdate(tmpStack, root(tmpStack))
    else root(tmpStack)

  given fSTreeEncoder: Encoder[FSTree] = deriveEncoder
  given fsTreeDecoder: Decoder[FSTree] = deriveDecoder
  given fsTreeConfigEncoder: Encoder[FSTreeConfig] = deriveEncoder
  given fsTreeConfigDecoder: Decoder[FSTreeConfig] = deriveDecoder

  def replaceRootFromFile(filename: String): Unit =
    root = readJson[FSTree](filename)

  def stacks: Set[List[String]] = root.stacks

  def stacksWithScores: Map[List[String], Double] = root.stacksWithScores

  /** A tree that stores the status of each node in the tree, and calculates the
    * score of each node based on the hits and misses of the node and its
    * children. The score is used to determine the promotion or demotion of a
    * node. The fields are mutable for (maybe) better performance than using
    * immutable case classes and copying.
    *
    * @param children
    * @param status
    * @param hits
    * @param misses
    * @param promotables
    *   number of Promotable descendants of this node.
    * @param avgScore
    *   average score of its descendants which are Promotable. If this node is
    *   Promotable, the score is calculated based on the hits and misses of this
    *   node.
    * @param avgScoreSq
    */
  case class FSTree(
    private val children: MMap[String, FSTree] = MMap.empty[String, FSTree],
    private var status: FSTreeStatus,
    private val depth: Int,
    var hits: Long = 0,
    var misses: Long = 0,
    var parentHits: Long = 0,
    var parentMisses: Long = 0,
    private var dirty: Boolean = false,
    private var promotables: Int = 0,
    private var chiSqValue: Double = 0,
  ) {
    import FSTreeStatus.*

    /** How many features do we need to take from the stack?
      *
      * @param stack
      *   the stack of features' names
      * @return
      *   the number of features we need to take
      */
    private[FSTreeWrapper] def apply(stack: List[String]): Int =
      math.min(
        foldByStack(stack, -1) {
          case (acc, node) =>
            if node.status == Noticed then acc + 1
            else acc
        },
        config.maxSensitivity,
      )

    /** Insert a feature stack into the tree. Increment the hits or misses of
      * each node in the tree based on whether the node is hit or miss.
      */
    @tailrec
    private[FSTreeWrapper] final def touchByStack(
      stack: List[String],
      isHit: Boolean,
    ): Unit =
      if isHit then hits += 1 else misses += 1
      dirty = true
      stack match {
        case Nil =>
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.touchByStack(tail, isHit)
            case None =>
              val child = new FSTree(
                MMap.empty[String, FSTree],
                status match {
                  case Noticed => Promotable
                  case _       => Ignored
                },
                depth + 1,
              )
              children(head) = child
              child.touchByStack(tail, isHit)
          }
      }

    /** Write back the scores and number of promotables of each node if it's
      * dirty
      */
    private[FSTreeWrapper] def writeback(): Unit =
      foreachFromRoot { node =>
        // update parentHits and parentMisses
        for (child <- node.children.valuesIterator) {
          child.parentHits = node.hits
          child.parentMisses = node.misses
        }
      }
      foreachFromLeaf { node =>
        // write back the if it's dirty
        if node.dirty then
          node.dirty = false
          node.status match {
            case Promotable => // calculate the chiSq based on hits and misses
              node.chiSqValue = computeFeatureChiSq(
                hits = node.hits,
                misses = node.misses,
                pHits = node.parentHits,
                pMisses = node.parentMisses,
              )
              node.promotables = 1
            case Noticed => // calculate the average score and number of the node's promotables
              node.promotables = node.children.valuesIterator
                .map(child => child.promotables)
                .sum
              node.chiSqValue = computeFeatureChiSq(
                hits = node.hits,
                misses = node.misses,
                pHits = node.parentHits,
                pMisses = node.parentMisses,
              )
            // if node.promotables == 0 then
            //   computeFeatureChiSq(
            //     hits = node.hits,
            //     misses = node.misses,
            //     pHits = node.parentHits,
            //     pMisses = node.parentMisses,
            //   )
            // else
            //   node.children.valuesIterator
            //     .map(child => child.chiSqValue * child.promotables)
            //     .sum / node.promotables
            case Ignored => ()
          }
          assert(
            node.chiSqValue >= 0,
            f"Score is negative: ${node.chiSqValue}, node: $node",
          )
      }

    /** Promote or demote each node in the tree based on the score of this node.
      * Assume that the children have already been written back.
      */
    private[FSTreeWrapper] def updateStatus(): Unit =
      if !chiSqValue.isFinite then println("Score is not finite")
      // demote to Ignored from the leaf first
      foreachFromLeaf { node =>
        node.status match {
          case Noticed if node.chiSqValue < config.demotionThreshold =>
            node.status = Ignored
            node.promotables = 0
            children.valuesIterator.foreach { child =>
              child.status = Ignored
            }
            sensDistr(node.depth) -= 1
          case _ => ()
        }
      }
      // promote from Promotable to Noticed and Ignored to Promotable
      foreachFromRoot { node =>
        node.status match {
          case Noticed =>
            children.valuesIterator.foreach { child =>
              child.status match
                case Ignored => child.status = Promotable
                case _       =>
            }
          case Promotable if node.chiSqValue > config.promotionThreshold =>
            node.status = Noticed
            sensDistr(node.depth) += 1
            node.children.valuesIterator.foreach { child =>
              child.status = Promotable
            }
          case _ => node.status
        }
      }

    /** Recursively do something to each node in the tree, starting from the
      * root
      */
    private def foreachFromRoot(
      op: FSTree => Unit,
      iterIgnored: Boolean = false,
    ): Unit =
      op(this)
      children.valuesIterator.foreach { child =>
        if iterIgnored || child.status != Ignored then
          child.foreachFromRoot(op, iterIgnored)
      }

    /** Recursively do something to each node in the tree, starting from the
      * leaf
      */
    private def foreachFromLeaf(
      op: FSTree => Unit,
      iterIgnored: Boolean = false,
    ): Unit =
      children.valuesIterator.foreach { child =>
        if iterIgnored || child.status != Ignored then
          child.foreachFromRoot(op, iterIgnored)
      }
      op(this)

    @tailrec
    private final def iterByStack(stack: List[String])(
      op: FSTree => Unit,
    ): Unit =
      op(this)
      stack match {
        case Nil =>
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.iterByStack(tail)(op)
            case None        =>
          }
      }

    @tailrec
    private final def foldByStack[A](stack: List[String], z: A)(
      op: (A, FSTree) => A,
    ): A =
      stack match {
        case Nil => op(z, this)
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.foldByStack(tail, op(z, this))(op)
            case None        => z
          }
      }

    private def get(stack: List[String]): Option[FSTree] =
      foldByStack(stack, Option.empty[FSTree])((_, node) => Some(node))

    private def touches: Long = hits + misses

    def stacks = stacksSuppl(Nil, Set.empty)

    private def stacksSuppl(
      currStack: List[String],
      acc: Set[List[String]],
    ): Set[List[String]] =
      if children.isEmpty then acc + currStack
      else if (status == Promotable) || (status == Ignored) then acc
      else
        children.flatMap {
          case (k, v) =>
            v.stacksSuppl(currStack :+ k, acc)
        }.toSet + currStack

    def stacksWithScores: Map[List[String], Double] =
      stacksWithScoresSuppl(Nil, Map.empty)

    def stacksWithScoresSuppl(
      currStack: List[String],
      acc: Map[List[String], Double],
    ): Map[List[String], Double] =
      if children.isEmpty then acc + (currStack -> chiSqValue)
      else if (status == Promotable) || (status == Ignored) then acc
      else
        children.flatMap {
          case (k, v) =>
            v.stacksWithScoresSuppl(currStack :+ k, acc)
        }.toMap + (currStack -> chiSqValue)
  }
}

case class FSTreeConfig(
  promotionThreshold: Double,
  demotionThreshold: Double,
  maxSensitivity: Int = 3,
  minTouch: Int = 10,
)

/** Status of a node in the tree
  *   - Noticed: actively noticed; will not be demoted
  *   - Promotable: ignored but might be promoted to Noticed
  *   - Ignored: not noticed; will not be promoted
  */
enum FSTreeStatus:
  case Noticed
  case Promotable
  case Ignored
