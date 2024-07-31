package esmeta.analyzer.domain.simpleValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.Appender.*

trait SimpleValueDomainDecl { self: Self =>

  /** abstract domain for simple values */
  trait SimpleValueDomain extends Domain[SimpleValue] {

    /** element constructors with Scala values */
    def apply(num: Double): Elem = alpha(Number(num))
    def apply(str: String): Elem = alpha(Str(str))
    def apply(bigInt: scala.math.BigInt): Elem = alpha(BigInt(bigInt))
    def apply(bool: Boolean): Elem = alpha(Bool(bool))

    /** predefined top values */
    def numberTop: Elem
    def bigIntTop: Elem
    def strTop: Elem
    def boolTop: Elem
    def undefTop: Elem
    def nullTop: Elem

    /** constructors */
    def apply(
      num: AbsNumber = AbsNumber.Bot,
      bigInt: AbsBigInt = AbsBigInt.Bot,
      str: AbsStr = AbsStr.Bot,
      bool: AbsBool = AbsBool.Bot,
      undef: AbsUndef = AbsUndef.Bot,
      nullv: AbsNull = AbsNull.Bot,
    ): Elem

    /** raw tuple of each simple value type */
    type RawTuple = (
      AbsNumber,
      AbsBigInt,
      AbsStr,
      AbsBool,
      AbsUndef,
      AbsNull,
    )

    /** extractors */
    def unapply(elem: Elem): Option[RawTuple]

    /** simple value element interfaces */
    extension (elem: Elem) {

      /** getters */
      def number: AbsNumber
      def bigInt: AbsBigInt
      def str: AbsStr
      def bool: AbsBool
      def undef: AbsUndef
      def nullv: AbsNull
    }
  }
}
