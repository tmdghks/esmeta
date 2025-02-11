package esmeta.es.util.fuzzer

import esmeta.error.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.injector.*
import esmeta.es.util.mutator.*
import esmeta.es.util.synthesizer.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{FUZZ_LOG_DIR, LINE_SEP}
import io.circe.*, io.circe.syntax.*
import java.io.PrintWriter
import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.collection.parallel.CollectionConverters.*
import scala.util.{Try, Success, Failure}

/** ECMAScript program fuzzer with ECMA-262 */
object Fuzzer {
  def apply(
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no-debug
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    kFs: Int = 0,
    cp: Boolean = false,
    logTranspilable: Boolean = false,
    transpiler: Option[List[String]] = None,
  ): Coverage = new Fuzzer(
    logInterval,
    debug,
    stdOut,
    timeLimit,
    trial,
    duration,
    kFs,
    cp,
    logTranspilable,
    transpiler,
  ).result

  // debugging levels
  val ALL = 2
  val PARTIAL = 1
  val NO_DEBUG = 0
}

/** extensible helper of ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  duration: Option[Int] = None, // `None` denotes no bound
  kFs: Int = 0,
  cp: Boolean = false,
  logTranspilable: Boolean = false, // log transpilable ratio
  transpiler: Option[List[String]] = None, // transpiler target
) {
  import Fuzzer.*

  /** generated ECMAScript programs */
  lazy val result: Coverage =
    logInterval.map(_ => {
      // start logging
      mkdir(logDir, remove = true)
      createSymlink(symlink, logDir, overwrite = true)
      dumpFile(getSeed, s"$logDir/seed")
      genSummaryHeader
      genStatHeader(selector.names, selStatTsv)
      genStatHeader(mutator.names, mutStatTsv)
    })
    val size = initPool.size

    if logTranspilable then
      debugging(s"- logging transpilable: $logTranspilable - ")
      debugging(
        s" transpiler: ${transpiler.map(_.mkString(", ")).getOrElse("None")}",
      )
      debugging("useSwc: " + useSwc)
      debugging("useTerser: " + useTerser)
      debugging("useBabel: " + useBabel)
      debugging("useSwcES2015: " + useSwcES2015)

    time(
      s"- initializing program pool with ${initPool.size} programs", {
        for {
          ((synthesizer, rawCode), i) <- initPool.toList.zipWithIndex
          code <- optional(scriptParser.from(rawCode).toString(grammar))
        } {
          debugging {
            val header = s"${synthesizer.name} (${i + 1}/$size)"
            f"[$header%-30s] $code"
          }
          add(code)
        }
      },
    )
    println(s"- the initial program pool consists of ${pool.size} programs.")
    time(
      "- repeatedly trying to fuzz new programs to increase coverage", {
        logInterval.map(_ => {
          startTime = System.currentTimeMillis
          startInterval = System.currentTimeMillis
          logging
        })
        trial match
          case Some(count) => for (_ <- Range(0, count)) if (!timeout) fuzz
          case None        => while (!timeout) fuzz
      },
    )
    // finish logging
    logInterval.map(_ => {
      logging
      summaryTsv.close
      selStatTsv.close
      mutStatTsv.close
    })
    cov

  /** current program pool */
  def pool: Set[Script] = cov.minimalScripts

  /** one trial to fuzz new programs to increase coverage */
  def fuzz: Unit =
    iter += 1
    debugging(("-" * 40) + f"  iter: $iter%10d  " + ("-" * 40))
    for (bound <- logInterval) {
      val seconds = bound * 1000
      if (interval > seconds) {
        if (debug == NO_DEBUG) logging else time("Logging", logging)
        startInterval += seconds
      }
    }
    val (selectorName, script, condView) = selector(pool, cov)
    val selectorInfo = selectorName + condView.map(" - " + _).getOrElse("")
    val code = script.code
    debugging(f"[$selectorInfo%-30s] $code")
    debugFlush

    val mutants = mutator(code, 100, condView.map((_, cov)))
      .map((name, ast) => (name, ast.toString(grammar)))
      .distinctBy(_._2)
      .toArray
      .par
      .map(infoExtractor)
      .toList

    for ((mutatorName, mutatedCode, info) <- mutants)
      debugging(f"----- $mutatorName%-20s-----> $mutatedCode")

      val result = add(mutatedCode, info)
      update(selectorName, selectorStat, result)
      update(mutatorName, mutatorStat, result)

  /** Case class to hold the information about mutant */
  case class MutantInfo(
    val visited: Boolean = false,
    val invalid: Boolean = false,
    val interp: Option[Coverage.Interp] = None,
  )

  /** Extract information for the mutated code. Should be side-effect free. */
  def infoExtractor(
    mutatorName: String,
    mutatedCode: String,
  ): (String, String, MutantInfo) =
    val info = {
      if (visited contains mutatedCode)
        MutantInfo(visited = true)
      else if (!ValidityChecker(mutatedCode))
        MutantInfo(invalid = true)
      else
        MutantInfo(interp = optional(cov.run(mutatedCode)))
    }
    (mutatorName, mutatedCode, info)

  /** add new program */
  def add(code: String): Boolean = handleResult(Try {
    if (visited contains code)
      fail("ALREADY VISITED")
    visited += code
    if (!ValidityChecker(code))
      fail("INVALID PROGRAM")
    val script = toScript(code)
    val (_, updated, covered) = cov.runAndCheck(script)
    if (!updated) fail("NO UPDATE")
    covered
  })

  /** add new program with precomputed info */
  def add(code: String, info: MutantInfo): Boolean = handleResult(Try {
    if (info.visited)
      fail("ALREADY VISITED")
    visited += code
    if (info.invalid)
      fail("INVALID PROGRAM")
    val script = toScript(code)
    val (_, updated, covered) =
      cov.check(script, info.interp.getOrElse(fail("Interp Fail")))
    if (!updated) fail("NO UPDATE")
    covered
  })

  /** handle add result */
  def handleResult(result: Try[Boolean]): Boolean =
    debugging(f" ${"COVERAGE RESULT"}%30s: ", newline = false)
    val pass = result match
      case Success(covered)             => debugging(passMsg("")); covered
      case Failure(e: TimeoutException) => debugging(failMsg("TIMEOUT")); false
      case Failure(e: NotSupported) =>
        debugging(failMsg("NOT SUPPORTED")); false
      case Failure(e) =>
        e.getMessage match
          case "ALREADY VISITED" | "INVALID PROGRAM" if debug == PARTIAL =>
            debugClean
          case msg =>
            debugging(failMsg(msg))
        false
    debugFlush
    pass

  // a pass-or-fail counter
  case class Counter(pass: Int = 0, fail: Int = 0)
  def update[T](t: T, map: MMap[T, Counter], pass: Boolean): Unit =
    val Counter(p, f) = map.getOrElse(t, Counter())
    val updated = if (pass) Counter(p + 1, f) else Counter(p, f + 1)
    map += t -> updated
  private def counterJson[T: Ordering](map: MMap[T, Counter]): Json =
    JsonObject(
      (for ((condView, Counter(pass, fail)) <- map.toList.sortBy(_._1)) yield {
        val key = condView.toString
        val obj = JsonObject(
          "pass" -> pass.asJson,
          "fail" -> fail.asJson,
        ).asJson
        key -> obj
      }): _*,
    ).asJson

  /** ECMAScript grammar */
  val grammar = cfg.grammar
  val scriptParser = cfg.scriptParser

  /** coverage */
  val cov: Coverage = Coverage(timeLimit, kFs, cp, logTranspilable, transpiler)

  /** target selector */
  val selector: TargetSelector = WeightedSelector(
    RandomSelector -> 2,
    BranchSelector -> 8,
  )

  /** selector stat */
  val selectorStat: MMap[String, Counter] = MMap()

  /** mutator */
  val mutator: Mutator = WeightedMutator(
    NearestMutator() -> 6,
    RandomMutator() -> 3,
    StatementInserter() -> 1,
    Remover() -> 1,
    SpecStringMutator() -> 1,
  )

  /** mutator stat */
  val mutatorStat: MMap[String, Counter] = MMap()

  /** initial pool */
  val initPool =
    SimpleSynthesizer.initPool.map(SimpleSynthesizer -> _) ++
    BuiltinSynthesizer.initPool.map(BuiltinSynthesizer -> _)

  lazy val logDir: String = s"$FUZZ_LOG_DIR/fuzz-$dateStr"
  lazy val symlink: String = s"$FUZZ_LOG_DIR/recent"

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // current iteration count
  private var iter: Int = 0

  // current id
  private var idCounter: Long = 0
  private def nextId: Long = { val id = idCounter; idCounter += 1; id }

  // evaluation start time
  private var startTime: Long = 0L
  private def elapsed: Long = System.currentTimeMillis - startTime
  private def timeout = duration.fold(false)(_ * 1000 < elapsed)
  private var startInterval: Long = 0L
  private def interval: Long = System.currentTimeMillis - startInterval

  // conversion from code string to `Script` object
  private def toScript(code: String): Script = Script(code, s"$nextId.js")

  // check if the added code is visited
  private var visited: Set[String] = Set()

  // indicating that add failed
  private def fail(msg: String) = throw Exception(msg)

  // debugging
  private var debugMsg = ""
  private def debugging(
    msg: String,
    newline: Boolean = true,
  ): Unit = if (debug == ALL) {
    if (newline) println(msg) else print(msg)
  } else if (debug > NO_DEBUG) {
    debugMsg += msg
    if (newline) debugMsg += LINE_SEP
  }
  private def debugClean: Unit = debugMsg = ""
  private def debugFlush: Unit = { print(debugMsg); debugClean }

  // generate headers
  private def genSummaryHeader =
    var header = Vector(
      "iter(#)",
      "time(ms)",
      "time(h:m:s)",
      "program(#)",
      "minimal(#)",
      "node(#)",
      "branch(#)",
    )
    if (logTranspilable)
      if (useSwc) header ++= Vector("swc-transpilable(%)")
      if (useTerser) header ++= Vector("terser-transpilable(%)")
      if (useBabel) header ++= Vector("babel-transpilable(%)")
      if (useSwcES2015) header ++= Vector("swcES2015-transpilable(%)")
    if (kFs > 0) header ++= Vector(s"sens-node(#)", s"sens-branch(#)")
    header ++= Vector("target-conds(#)")
    if (kFs > 0) header ++= Vector(s"sens-target-conds(#)")
    addRow(header)
  private def genStatHeader(keys: List[String], nf: PrintWriter) =
    var header1 = Vector("iter(#)")
    var header2 = Vector("-")
    keys.foreach(k => {
      header1 ++= Vector(k, "-", "-", "-")
      header2 ++= Vector("pass", "fail", "total", "ratio")
    })
    addRow(header1, nf)
    addRow(header2, nf)

  // dump selector and mutator stat
  private def dumpStat(
    keys: List[String],
    stat: MMap[String, Counter],
    tsv: PrintWriter,
  ): Unit =
    var row = Vector[Any](iter)
    keys.foreach(k => {
      val Counter(pass, fail) = stat.getOrElse(k, Counter())
      val total = pass + fail
      val ratio = optional((pass * 10000) / total / 100.0).getOrElse(0.0)
      row ++= Vector(pass, fail, total, s"$ratio%")
    })
    addRow(row, tsv)

  // logging
  private def logging: Unit =
    val n = cov.nodeCov
    val b = cov.branchCov
    val e = elapsed
    val t = Time(e).simpleString
    val nv = cov.nodeViewCov
    val bv = cov.branchViewCov
    val tc = cov.targetCondViews.size
    val tcv = cov.targetCondViews.map(_._2.size).fold(0)(_ + _)
    lazy val swcTr = (cov.swcTranspilableRate * 100 * 1000).round / 1000.0
    lazy val terserTr = (cov.terserTranspilableRate * 100 * 1000).round / 1000.0
    lazy val swcES2015Tr =
      (cov.swcES2015TranspilableRate * 100 * 1000).round / 1000.0
    lazy val babelTr = (cov.babelTranspilableRate * 100 * 1000).round / 1000.0

    var row = Vector[Int | Long | String | Double](
      iter,
      e,
      t,
      visited.size,
      pool.size,
      n,
      b,
    )
    if (logTranspilable)
      if (useSwc) row ++= Vector(swcTr)
      if (useTerser) row ++= Vector(terserTr)
      if (useBabel) row ++= Vector(babelTr)
      if (useSwcES2015) row ++= Vector(swcES2015Tr)
    if (kFs > 0) row ++= Vector(nv, bv)
    row ++= Vector(tc)
    if (kFs > 0) row ++= Vector(tcv)
    addRow(row)
    // dump coveragge
    cov.dumpToWithDetail(logDir, withMsg = (debug == ALL))
    // dump selector and mutator stat
    dumpStat(selector.names, selectorStat, selStatTsv)
    dumpStat(mutator.names, mutatorStat, mutStatTsv)
  private def addRow(data: Iterable[Any], nf: PrintWriter = summaryTsv): Unit =
    val row = data.mkString("\t")
    if (stdOut) println(row)
    nf.println(row)
    nf.flush
  private lazy val summaryTsv: PrintWriter = getPrintWriter(
    s"$logDir/summary.tsv",
  )
  private lazy val selStatTsv: PrintWriter = getPrintWriter(
    s"$logDir/selector-stat.tsv",
  )
  private lazy val mutStatTsv: PrintWriter = getPrintWriter(
    s"$logDir/mutation-stat.tsv",
  )

  // ---------------------------------------------------------------------------
  private lazy val useSwc = transpiler.exists(_.contains("swc"))
  private lazy val useTerser = transpiler.exists(_.contains("terser"))
  private lazy val useBabel = transpiler.exists(_.contains("babel"))
  private lazy val useSwcES2015 = transpiler.exists(_.contains("swcES2015"))
}
