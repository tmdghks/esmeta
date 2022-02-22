package esmeta.interp

import esmeta.cfg.*
import esmeta.interp.util.*
import esmeta.ir.{Func => IRFunc, *}

/** IR calling contexts */
case class CallContext(retId: Id, context: Context) extends InterpElem {

  /** function name * */
  def name: String = context.func.ir.name

  /** copy contexts */
  def copied: CallContext = copy(context = context.copied)
}
