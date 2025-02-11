package esmeta.es.util.fuzzer

import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.{error => _, *}
import esmeta.es.Script
import esmeta.es.util.JsonProtocol
import esmeta.es.util.{Coverage, SelectiveCoverage}
import esmeta.es.util.fuzzer.Fuzzer.NO_DEBUG
import esmeta.interpreter.*
import esmeta.state.State
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.es.util.USE_STRICT
import scala.util.*
import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.{Map => MMap, Set => MSet}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

import io.circe.Json
import java.util.concurrent.atomic.AtomicInteger
// import esmeta.mutator.TracerExprMutator
import esmeta.phase.SelectiveFuzz

object SelectiveFuzzer {
  def apply(
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    init: Option[String] = None,
    cp: Boolean = false,
    selectiveConfig: SelectiveConfig,
  ): Coverage = new SelectiveFuzzer(
    logInterval = logInterval,
    debug = debug,
    stdOut = stdOut,
    timeLimit = timeLimit,
    trial = trial,
    duration = duration,
    cp = cp,
    selectiveConfig = selectiveConfig,
  ).result

  def logDir(transpileCmd: String): String =
    s"$SELECTIVE_FUZZ_LOG_DIR/fuzz-$dateStr-$transpileCmd"
  val symlink: String = s"$SELECTIVE_FUZZ_LOG_DIR/recent"
}

class SelectiveFuzzer(
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  duration: Option[Int] = None, // `None` denotes no bound
  cp: Boolean = false,
  selectiveConfig: SelectiveConfig,
) {
  import SelectiveFuzzer.*

  lazy val result: Coverage = fuzzer.result

  val filteredAOs: List[String] = List(
    "INTRINSICS.Function.prototype.toString",
  )

  lazy val fuzzer = new Fuzzer(
    logInterval = logInterval,
    debug = debug,
    timeLimit = timeLimit,
    trial = trial,
    duration = duration,
    kFs = selectiveConfig.maxSensitivity,
    cp = cp,
  ) {
    override lazy val logDir =
      SelectiveFuzzer.logDir(selectiveConfig.targetTrans)
    override lazy val symlink = SelectiveFuzzer.symlink

    // adjust weight for active random fuzzing
    override val selector: TargetSelector = WeightedSelector(
      RandomSelector -> 2,
      BranchSelector -> 8,
    )

    override val cov: SelectiveCoverage = SelectiveCoverage(
      timeLimit = timeLimit,
      cp = cp,
      selectiveConfig = selectiveConfig,
    )

    // TODO: Modify logging with TargetFeatureSet
    override def logging: Unit =
      val startTime = System.currentTimeMillis

      val n = cov.nodeCov
      val b = cov.branchCov
      val e = elapsed
      val t = Time(e).simpleString
      val nv = cov.nodeViewCov
      val bv = cov.branchViewCov
      val tc = cov.targetCondViews.size
      val tcv = cov.targetCondViews.map(_._2.size).fold(0)(_ + _)
      val mr = (cov.transpilableRate * 100 * 1000).round / 1000.0
      var row = Vector(iter, e, t, visited.size, pool.size, n, b, mr)
      val targetFeatureSize = cov.targetFeatSet.targetFeatureSize
      row ++= Vector(targetFeatureSize)
      if (selectiveConfig.maxSensitivity > 0) row ++= Vector(nv, bv)
      row ++= Vector(tc)
      if (selectiveConfig.maxSensitivity > 0) row ++= Vector(tcv)
      addRow(row)
      // dump coverage
      cov.dumpToWithDetail(logDir, withMsg = (debug == Fuzzer.ALL))
      // dump selector and mutator stat
      dumpStat(selector.names, selectorStat, selStatTsv)
      dumpStat(mutator.names, mutatorStat, mutStatTsv)

      val duration = Time(System.currentTimeMillis - startTime)
      println(f"- Logging time: $duration")
    // val jsonProtocol: JsonProtocol = JsonProtocol(cfg)
    // import jsonProtocol.given
    // super.logging
  }

}
