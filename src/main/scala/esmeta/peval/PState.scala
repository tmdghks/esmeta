package esmeta.peval

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}
import scala.util.{Try, Success}

import esmeta.state.*

/** IR PStates */
case class PState(
  val globals: Map[Global, Predict[Value]],
  var callStack: List[PContext],
  val context: PContext,
  val heap: PHeap,
) extends StateElem {

  inline def func = context.func
  inline def locals = context.locals

  /** getter */
  def apply(rt: Predict[RefTarget])(using CFG): Predict[Value] = rt match
    case Known(rt) => apply(rt)
    case Unknown   => Unknown

  /** getter */
  def apply(rt: RefTarget)(using CFG): Predict[Value] =
    rt match
      case VarTarget(x)             => apply(x)
      case FieldTarget(base, field) => apply(base, field)

  /** variable getter */
  def apply(x: Var): Predict[Value] = x match
    case x: Global => globals.getOrElse(x, Unknown)
    case x: Local  => locals.getOrElse(x, throw UnknownVar(x))

  /** field getter */
  def apply(
    base: Value,
    field: Value,
  )(using CFG): Predict[Value] = base match
    case addr: Addr =>
      if heap(addr).exists(field) then heap(addr, field)
      else Unknown
    case AstValue(ast) => Known(AstValue(ast(field)))
    case Str(str)      => apply(str, field)
    case v             => throw InvalidRefBase(v)

  /** string field getter */
  def apply(str: String, field: Value): Predict[Value] = field match
    case Math(k) => Known(CodeUnit(str(k.toInt)))
    case _       => throw WrongStringRef(str, field)

  /** address getter */
  def apply(addr: Addr): PObj = heap(addr)

  /** define variables */
  def define(x: Var, value: Predict[Value]): Unit = x match
    case x: Global => /* do nothing */
    case x: Local  => locals += x -> value

  /** setter */
  def update(rt: RefTarget, value: Predict[Value]): Unit = rt match
    case VarTarget(x)             => update(x, value)
    case FieldTarget(base, field) => update(base, field, value)

  /** variable setter */
  def update(x: Var, value: Predict[Value]): Unit = x match
    case x: Global => /* do nothing */
    case x: Local  => locals += x -> value

  /** field setter */
  def update(base: Value, field: Value, value: Predict[Value]): PState = ???
  // heap.update(base.asAddr, field, value)

  /** existence checks */
  def exists(rt: RefTarget): Predict[Boolean] = rt match
    case VarTarget(x) => exists(x)
    case FieldTarget(base, field) =>
      base match
        case addr: Addr    => ??? // heap.exists(addr, field)
        case AstValue(ast) => ??? // ast.exists(field)
        case _ => error(s"illegal field existence check: $base[$field]")

  /** variable existence check */
  def exists(x: Var): Predict[Boolean] = x match
    case x: Global => if globals.contains(x) then Known(true) else Unknown
    case x: Local  => Known(locals.contains(x))

  /** expand a field of a record object */
  def expand(base: Value, field: Value): Unit =
    ??? // heap.expand(base.asAddr, field)

  /** delete a key from an map object */
  def delete(base: Value, key: Value): Unit =
    ??? // heap.delete(base.asAddr, key)

  /** push a value to a list */
  def push(addr: Addr, value: Value, front: Boolean): Unit = ??? //
  // heap.push(addr, value, front)

  /** pop a value from a list */
  def pop(addr: Addr, front: Boolean): Value = ??? // heap.pop(addr, front)

  /** copy object */
  def copy(addr: Addr): Addr = ??? // heap.copy(addr)

  /** get keys of a record/map object as a list */
  def keys(addr: Addr, intSorted: Boolean): Addr =
    ??? // heap.keys(addr, intSorted)

  /** allocate a record object */
  def allocRecord(
    tname: String,
    pairs: Iterable[(String, Predict[Value])] = Nil,
  ): Addr = heap.allocRecord(tname, pairs)

  /** allocate a map object */
  def allocMap(pairs: Iterable[(Value, Predict[Value])]): (Addr, PState) = ???
  // val addr -> pheap = heap.allocMap(pairs)
  // (addr, this.replaced(heap = pheap))

  /** allocate a list object */
  def allocList(vs: Iterable[Predict[Value]]): (Addr, PState) = ???
  // val addr -> pheap = heap.allocList(vs)
  // (addr, this.replaced(heap = pheap))
}