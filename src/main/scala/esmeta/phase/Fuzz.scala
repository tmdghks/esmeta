package esmeta.phase

import esmeta.CommandConfig
import esmeta.cfg.CFG
import esmeta.es.util.Coverage
import esmeta.es.util.fuzzer.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `fuzz` phase */
case object Fuzz extends Phase[CFG, Coverage] {
  val name = "fuzz"
  val help = "generates JavaScript programs via fuzzing."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage = {
    // optionally set the seed for the random number generator
    config.seed.foreach(setSeed)

    Fuzzer(
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      kFs = config.kFs,
      cp = config.cp,
      logTranspilable = config.logTranspilable,
      transpiler = config.transpiler,
    )
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log-interval",
      NumOption((c, k) => c.logInterval = Some(k)),
      "turn on logging mode and set logging interval (default: 600 seconds).",
    ),
    (
      "log-transpilable",
      BoolOption(c => c.logTranspilable = true),
      "turn on logging ratio of transpilable programs.",
    ),
    (
      "transpiler",
      StrListOption((c, k) => c.transpiler = Some(k)),
      "set transpiler (default: None).",
    ),
    (
      "debug",
      NumOption((c, k) =>
        if (k < 0 || k > 2) error("invalid debug level: please set 0 to 2")
        else c.debug = k,
      ),
      "turn on deug mode with level (0: no-debug, 1: partial, 2: all)",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: 1 second).",
    ),
    (
      "trial",
      NumOption((c, k) => c.trial = Some(k)),
      "set the number of trials (default: INF).",
    ),
    (
      "duration",
      NumOption((c, k) => c.duration = Some(k)),
      "set the maximum duration for fuzzing (default: INF)",
    ),
    (
      "seed",
      NumOption((c, k) => c.seed = Some(k)),
      "set the specific seed for the random number generator (default: None).",
    ),
    (
      "k-fs",
      NumOption((c, k) => c.kFs = k),
      "set the k-value for feature sensitivity (default: 0).",
    ),
    (
      "cp",
      BoolOption(c => c.cp = true),
      "turn on the call-path mode (default: false) (meaningful if k-fs > 0).",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var logInterval: Option[Int] = Some(600),
    var logTranspilable: Boolean = false,
    var transpiler: Option[List[String]] = None,
    var debug: Int = 0,
    var timeLimit: Option[Int] = Some(1),
    var trial: Option[Int] = None,
    var duration: Option[Int] = None,
    var seed: Option[Int] = None,
    var kFs: Int = 0,
    var cp: Boolean = false,
  )
}
