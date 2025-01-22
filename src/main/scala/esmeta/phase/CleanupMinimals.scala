package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.es.util.USE_STRICT
import esmeta.es.util.fuzzer.{MinifierDB, MinifyChecker}
import esmeta.es.Script
import esmeta.js.minifier.Minifier
import scala.util.*
import java.util.concurrent.atomic.AtomicLong
import esmeta.util.SystemUtils.*
import scala.collection.parallel.CollectionConverters._
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

case object CleanupMinimals extends Phase[CFG, Unit] {
  val name = "cleanup-minimals"
  val help = "cleanup minimals given fstree"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    import esmeta.ty.util.JsonProtocol.given

    given ConInvDataEncoder: Encoder[CovInvData] = deriveEncoder

    val covDir = getFirstFilename(cmdConfig, "minimal-cleanup")

    val cov = Coverage.fromLogSimpl(covDir, cfg)

    cov.dumpToWithDetail(
      config.out.getOrElse(s"$covDir-cleanup"),
      silent = true,
    )

    println(
      s"Cleanup minimals done. Output: ${config.out.getOrElse(s"$covDir-cleanup")}",
    )
    println(s"Total minimals: ${cov.minimalScripts.size}")

    ()

  val defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
  )

  class Config(
    var out: Option[String] = None,
  )
}
