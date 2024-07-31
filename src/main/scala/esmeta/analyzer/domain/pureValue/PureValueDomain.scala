package esmeta.analyzer.domain.pureValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait PureValueDomainDecl { self: Self =>

  /** abstract pure value (value except completion record) domain */
  trait PureValueDomain extends Domain[APureValue] {

    /** abstraction functions for an original pure value */
    def alpha(pureValue: PureValue): Elem = alpha(APureValue.from(pureValue))

    /** predefined top values */
    def cloTop: Elem
    def contTop: Elem
    def partTop: Elem
    def astValueTop: Elem
    def grammarSymbolTop: Elem
    def codeUnitTop: Elem
    def enumTop: Elem
    def mathTop: Elem
    def infinityTop: Elem
    def simpleValueTop: Elem
    def numberTop: Elem
    def bigIntTop: Elem
    def strTop: Elem
    def boolTop: Elem
    def undefTop: Elem
    def nullTop: Elem

    /** constructors */
    def apply(
      clo: AbsClo = AbsClo.Bot,
      cont: AbsCont = AbsCont.Bot,
      part: AbsPart = AbsPart.Bot,
      astValue: AbsAstValue = AbsAstValue.Bot,
      grammarSymbol: AbsGrammarSymbol = AbsGrammarSymbol.Bot,
      codeUnit: AbsCodeUnit = AbsCodeUnit.Bot,
      enumv: AbsEnum = AbsEnum.Bot,
      math: AbsMath = AbsMath.Bot,
      infinity: AbsInfinity = AbsInfinity.Bot,
      simpleValue: AbsSimpleValue = AbsSimpleValue.Bot,
      number: AbsNumber = AbsNumber.Bot,
      bigInt: AbsBigInt = AbsBigInt.Bot,
      str: AbsStr = AbsStr.Bot,
      bool: AbsBool = AbsBool.Bot,
      undef: AbsUndef = AbsUndef.Bot,
      nullv: AbsNull = AbsNull.Bot,
    ): Elem

    /** raw tuple of each simple value type */
    type RawTuple = (
      AbsClo,
      AbsCont,
      AbsPart,
      AbsAstValue,
      AbsGrammarSymbol,
      AbsCodeUnit,
      AbsEnum,
      AbsMath,
      AbsInfinity,
      AbsSimpleValue,
    )

    /** extractors */
    def unapply(elem: Elem): Option[RawTuple]

    /** element interfaces */
    extension (elem: Elem) {

      /** getters */
      def clo: AbsClo
      def cont: AbsCont
      def part: AbsPart
      def astValue: AbsAstValue
      def grammarSymbol: AbsGrammarSymbol
      def codeUnit: AbsCodeUnit
      def enumv: AbsEnum
      def math: AbsMath
      def infinity: AbsInfinity
      def simpleValue: AbsSimpleValue
      def number: AbsNumber
      def bigInt: AbsBigInt
      def str: AbsStr
      def bool: AbsBool
      def undef: AbsUndef
      def nullv: AbsNull

      /** get reachable address partitions */
      def reachableParts: Set[Part]
    }
  }
}
