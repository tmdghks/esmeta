package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.Ast
import esmeta.js.builtin.SOURCE_TEXT

/** abstract semantics initializer */
object Initialize {

  /** initialize JavaScript analysis */
  def initJs(sourceText: String): Map[NodePoint[Node], AbsState] = {
    // initial control point
    val initCp = {
      val runJobs = cfg.fnameMap("RunJobs")
      val entry = runJobs.entry.get
      NodePoint(runJobs, entry, View())
    }
    Map(
      initCp -> AbsState.Empty.defineGlobal(
        Global(SOURCE_TEXT) -> AbsValue(sourceText),
      ),
    )
  }

  /** initialize type analysis */
  private lazy val sdoPattern = """(\w+)\[(\d+),(\d+)\]\.\w+""".r

  var counter = 0

  def initType(cfg: CFG): Map[NodePoint[Node], AbsState] = (for {
    f <- cfg.funcs
    (np, st) <- f.irFunc.kind match

      case _ if counter >= 1 => None

      case IRFunc.Kind.SynDirOp if f.irFunc.params.length == 1 =>
        val sdoPattern(name, idx, subIdx) = f.name
        val thisTy = SyntacticT(name, idx.toInt, subIdx.toInt)
        val np = NodePoint(f, f.entry.get, View(tys = List(thisTy)))
        val st = AbsState.Empty.defineLocal(THIS -> AbsValue(thisTy))

        counter += 1

        Some(np, st)
      case IRFunc.Kind.Builtin =>
        val np = NodePoint(f, f.entry.get, View())
        val st = AbsState.Empty
          .defineLocal(
            THIS -> ES_VALUE_TYPE,
            ARGUMENTS_LIST -> LIST_ES_VALUE_TYPE,
            NEW_TARGET -> NEW_TARGET_TYPE,
          )

        counter += 1

        Some(np, st)
      case _ => None
  } yield np -> st).toMap

  /** alias */
  lazy val THIS = Name("this")
  lazy val ARGUMENTS_LIST = Name("argumentsList")
  lazy val NEW_TARGET = Name("NewTarget")
  lazy val ES_VALUE_TYPE = AbsValue(ESValueT)
  lazy val LIST_ES_VALUE_TYPE = AbsValue(ListT(ESValueT))
  lazy val NEW_TARGET_TYPE = AbsValue(NameT("Object"), UndefT)

  // // TODO remove
  // def typeAnalysisTest(): AbsSemantics = {
  //   val initCp = {
  //     val toBoolean = cfg.fnameMap("ToBoolean")
  //     val entry = toBoolean.entry.get
  //     NodePoint(toBoolean, entry, View())
  //   }
  //   AbsSemantics(
  //     npMap = Map(
  //       initCp -> AbsState.Empty.defineLocal(
  //         Name("argument") -> AbsValue.undef,
  //       ),
  //     ),
  //   )
}