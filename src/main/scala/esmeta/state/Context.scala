package esmeta.state

import esmeta.cfg.{Func, Block, Call}
import esmeta.ir.{Func => IRFunc, *}
import esmeta.es.Ast
import esmeta.util.BaseUtils.error
import scala.collection.mutable.{Map => MMap}

/** IR contexts */
case class Context(
  val func: Func,
  val locals: MMap[Local, Value] = MMap(),
) extends StateElem {

  /** current cursor in this context */
  var cursor: Cursor = NodeCursor(func.entry)

  /** move cursor to next */
  def moveNext: Unit = cursor match
    case NodeCursor(block: Block) => cursor = Cursor(block.next, func)
    case NodeCursor(call: Call)   => cursor = Cursor(call.next, func)
    case _                        => cursor = ExitCursor(func)

  /** return variable */
  var retVal: Option[(Return, Value)] = None

  /** copy contexts */
  def copied: Context = {
    val newContext = copy(locals = MMap.from(locals))
    newContext.cursor = cursor
    newContext
  }

  /** name */
  def name: String = func.irFunc.name

  /** ast of current context */
  def astOpt: Option[Ast] =
    if (func.isSDO) Some(locals(NAME_THIS).asAst)
    else None
}
