package esmeta.es.util.fuzzer

import esmeta.{error => _, *}
import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.es.util.fuzzer.Fuzzer.NO_DEBUG
import esmeta.es.util.Coverage
import esmeta.injector.*
import esmeta.state.State
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.atomic.AtomicLong
import scala.util.*
import scala.collection.parallel.CollectionConverters._
import esmeta.js.minifier.Minifier
import esmeta.js.JSEngine
import io.circe.Json
import esmeta.es.util.USE_STRICT

object MinifyFuzzer {
  def apply(
    cfg: CFG,
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    init: Option[String] = None,
    kFs: Int = 0,
    cp: Boolean = false,
  ): Coverage = new MinifyFuzzer(
    cfg,
    logInterval,
    debug,
    stdOut,
    timeLimit,
    trial,
    duration,
    init,
    kFs,
    cp,
  ).result
}

class MinifyFuzzer(
  cfg: CFG,
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  duration: Option[Int] = None, // `None` denotes no bound
  init: Option[String] = None,
  kFs: Int = 0,
  cp: Boolean = false,
) {
  val counter = AtomicLong(0)
  val logDir: String = s"$MINIFY_FUZZ_LOG_DIR/fuzz-$dateStr"
  val symlink: String = s"$MINIFY_FUZZ_LOG_DIR/recent"

  lazy val result: Coverage = fuzzer.result

  val filteredAOs: List[String] = List(
    "INTRINSICS.Function.prototype.toString",
  )

  var foundBugs: List[String] = List()

  lazy val fuzzer = new Fuzzer(
    cfg = cfg,
    logInterval = logInterval,
    debug = debug,
    timeLimit = timeLimit,
    trial = trial,
    duration = duration,
    kFs = kFs,
    cp = cp,
    logDir = logDir,
    symlink = symlink,
    init = init,
  ) {
    override def add(code: String, info: CandInfo): Boolean = handleResult(
      Try {
        if (info.visited)
          fail("ALREADY VISITED")
        visited += code
        if (info.invalid)
          fail("INVALID PROGRAM")
        val script = toScript(code)
        val interp = info.interp.getOrElse(fail("Interp Fail"))
        val finalState = interp.result
        val (_, updated, covered) = cov.check(script, interp)
        val filtered = interp.coveredAOs intersect filteredAOs
        if (filtered.isEmpty)
          beforeUpdate(iter, finalState, code, covered)
        else println(s"PASS minifier check due to: $filtered")
        if (!updated) fail("NO UPDATE")
        covered
      },
    )
  }

  private def beforeUpdate(
    iter: Int,
    finalState: State,
    code: String,
    covered: Boolean,
  ) =
    val injector = ReturnInjector(cfg, finalState, timeLimit, false)
    injector.exitTag match
      case NormalTag =>
        val returns = injector.assertions
        // TODO: some simple programs cannot be checked by this logic due to the empty return assertion
        for (ret <- returns.par) {
          val wrapped = s"${USE_STRICT}const k = (() => {\n$code\n$ret\n})();\n"
          Minifier.minifySwc(wrapped) match
            case Failure(exception) => println(s"[minify-fuzz] $exception")
            case Success(minified) => {
              val injected =
                Injector.replaceBody(
                  cfg,
                  wrapped,
                  minified,
                  defs = true,
                  timeLimit = timeLimit,
                  log = false,
                  ignoreProperties = "\"name\"" :: Nil,
                )
              injected.exitTag match
                case NormalTag =>
                  val code = injected.toString
                  JSEngine.runGraal(code, Some(1000)) match
                    case Success(v) if v.isEmpty =>
                      println(s"[minify-fuzz] pass")
                    case Success(v) =>
                      println(s"[minify-fuzz] return value exists")
                      log(iter, covered, wrapped, minified, code, v)
                    case Failure(exception) =>
                      println(s"[minify-fuzz] bug #${counter.get()}")
                      log(
                        iter,
                        covered,
                        wrapped,
                        minified,
                        code,
                        exception.toString,
                      )
                case _ => println("[minify-fuzz] exit state is not normal")
            }
        }
      case _ =>

  private def log(
    iter: Int,
    covered: Boolean,
    original: String,
    minified: String,
    injected: String,
    exception: String,
  ) = {
    val count = counter.incrementAndGet()
    val dirpath = s"$logDir/$count"
    mkdir(dirpath)
    dumpFile(original, s"$dirpath/original.js")
    dumpFile(minified, s"$dirpath/minified.js")
    dumpFile(injected, s"$dirpath/injected.js")
    dumpFile(exception, s"$dirpath/reason")
    dumpJson(
      Json.obj(
        "iter" -> Json.fromInt(iter),
        "covered" -> Json.fromBoolean(covered),
      ),
      s"$dirpath/info",
    )
  }

}