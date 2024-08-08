package esmeta.analyzer.domain.state

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.es.Initialize
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.StateMonad

trait StateDomainDecl { self: Self =>

  /** abstract state domain */
  trait StateDomain extends Domain[State] {

    /** empty state */
    def Empty: Elem

    /** monad helper */
    val monad: StateMonad[Elem] = StateMonad[Elem]()

    /** set bases */
    def setBase(init: Initialize): Unit

    /** abstract state interfaces */
    extension (elem: Elem) {

      /** getter */
      def get(rt: AbsRefTarget): AbsValue = rt match
        case AbsVarTarget(x)             => get(x)
        case AbsFieldTarget(base, field) => get(base, field)

      /** variable getter */
      def get(x: Var): AbsValue

      /** field getter */
      def get(base: AbsValue, field: AbsValue): AbsValue

      /** address getter */
      def get(part: Part): AbsObj

      /** define variables */
      def define(x: Var, value: AbsValue): Elem

      /** setter */
      def update(rt: AbsRefTarget, value: AbsValue): Elem = rt match
        case AbsVarTarget(x)             => update(x, value)
        case AbsFieldTarget(base, field) => update(base, field, value)

      /** identifier setter */
      def update(x: Var, value: AbsValue): Elem

      /** field setter */
      def update(base: AbsValue, field: AbsValue, value: AbsValue): Elem

      /** existence check */
      def exists(rt: AbsRefTarget): AbsValue = rt match
        case AbsVarTarget(x)             => exists(x)
        case AbsFieldTarget(base, field) => exists(base, field)

      /** variable existence check */
      def exists(x: Var): AbsValue

      /** field existence check */
      def exists(base: AbsValue, field: AbsValue): AbsValue

      /** expand a field of a record object */
      def expand(base: AbsValue, field: AbsValue): Elem

      /** delete a key from an map object */
      def delete(base: AbsValue, field: AbsValue): Elem

      /** push a value to a list */
      def push(list: AbsValue, value: AbsValue, front: Boolean): Elem

      /** pop a value from a list */
      def pop(list: AbsValue, front: Boolean): (AbsValue, Elem)

      /** copy object */
      def copy(from: AbsValue)(asite: AllocSite): (AbsValue, Elem)

      /** get keys of a record/map object as a list */
      def keys(
        obj: AbsValue,
        intSorted: Boolean,
      )(asite: AllocSite): (AbsValue, Elem)

      /** allocate a record object */
      def allocRecord(
        tname: String,
        pairs: Iterable[(String, AbsValue)] = Nil,
      )(asite: AllocSite): (AbsValue, Elem)

      /** allocate a map object */
      def allocMap(
        pairs: Iterable[(AbsValue, AbsValue)] = Nil,
      )(asite: AllocSite): (AbsValue, Elem)

      /** allocate a list object */
      def allocList(
        vs: Iterable[AbsValue] = Nil,
      )(asite: AllocSite): (AbsValue, Elem)

      /** check contains */
      def contains(list: AbsValue, value: AbsValue): AbsValue

      /** handle returns (elem: return states / to: caller states) */
      def doReturn(to: Elem, lhs: Local, value: AbsValue): Elem

      /** singleton address partition checks */
      def isSingle(part: Part): Boolean

      /** set local environments */
      def setLocal(locals: Map[Local, AbsValue]): Elem

      /** conversion to string */
      def getString(detail: Boolean): String

      /** get string with detailed shape of address partitions */
      def getString(value: AbsValue): String

      /** getters */
      def reachable: Boolean
      def locals: Map[Local, AbsValue]
      def globals: Map[Global, AbsValue]
      def heap: AbsHeap

      // -----------------------------------------------------------------------
      // Helpers for Debugging
      // -----------------------------------------------------------------------

      /** find merged parts */
      def findMerged: Unit

      /** get reachable address partitions */
      def reachableParts: Set[Part]

      // -----------------------------------------------------------------------
      // Helpers for Bottom Check
      // -----------------------------------------------------------------------

      /** check bottom elements in abstract semantics */
      protected def bottomCheck[A](dom: Domain[A])(
        vs: dom.Elem*,
      )(f: => Elem): Elem = elem.bottomCheck(dom)(vs)(f)
      protected def bottomCheck[A](dom: Domain[_])(
        vs: Iterable[dom.Elem],
      )(f: => Elem): Elem =
        if (elem.isBottom || vs.exists(_.isBottom)) Bot else f
      protected def bottomCheck[A](dom: Domain[A])(
        vs: dom.Elem*,
      )(f: => (AbsValue, Elem)): (AbsValue, Elem) = elem.bottomCheck(dom)(vs)(f)
      protected def bottomCheck[A](dom: Domain[A])(
        vs: Iterable[dom.Elem],
      )(f: => (AbsValue, Elem)): (AbsValue, Elem) =
        if (elem.isBottom || vs.exists(_.isBottom)) (AbsValue.Bot, Bot)
        else f
    }
  }
}
