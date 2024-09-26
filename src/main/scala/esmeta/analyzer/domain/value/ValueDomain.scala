package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.es.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.ir.{COp, Name, VOp, MOp, Local}
import esmeta.util.*

trait ValueDomainDecl { self: Self =>

  /** abstract valude domain */
  trait ValueDomain extends Domain[AValue] {

    /** abstraction functions for an original value */
    def apply(value: Value): Elem = alpha(AValue.from(value))

    /** constructor with types */
    def apply(ty: Ty): Elem = apply(ty, Map.empty)
    def apply(ty: Ty, guard: TypeGuard): Elem

    /** abstraction functions for raw data */
    def apply(ast: Ast): Elem = apply(AstValue(ast))
    def apply(n: Double): Elem = apply(Number(n))
    def apply(s: String): Elem = apply(Str(s))
    def apply(b: Boolean): Elem = apply(Bool(b))
    def apply(d: BigDecimal): Elem = apply(Math(d))

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
      num: AbsNumber = AbsNumber.Bot,
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

    /** transfer for variadic operation */
    def vopTransfer(vop: VOp, vs: List[Elem]): Elem

    /** transfer for mathematical operation */
    def mopTransfer(mop: MOp, vs: List[Elem]): Elem

    /** abstract value interfaces */
    extension (elem: Elem) {

      /** get key values */
      def keyValue: Elem

      /** bitwise operations */
      def &(that: Elem): Elem
      def |(that: Elem): Elem
      def ^(that: Elem): Elem

      /** comparison operations */
      def =^=(that: Elem): Elem
      def ==^==(that: Elem): Elem
      def <(that: Elem): Elem

      /** logical operations */
      def &&(that: Elem): Elem
      def ||(that: Elem): Elem
      def ^^(that: Elem): Elem

      /** numeric operations */
      def +(that: Elem): Elem
      def sub(that: Elem): Elem
      def /(that: Elem): Elem
      def *(that: Elem): Elem
      def %(that: Elem): Elem
      def %%(that: Elem): Elem
      def **(that: Elem): Elem
      def <<(that: Elem): Elem
      def >>>(that: Elem): Elem
      def >>(that: Elem): Elem

      /** unary operations */
      def unary_- : Elem
      def unary_! : Elem
      def unary_~ : Elem
      def abs: Elem
      def floor: Elem

      /** type operations */
      def typeOf(st: AbsState): Elem
      def typeCheck(ty: Ty, st: AbsState): Elem

      /** helper functions for abstract transfer */
      def convertTo(cop: COp, radix: Elem): Elem
      def sourceText: Elem
      def parse(rule: Elem): Elem
      def substring(from: Elem): Elem
      def substring(from: Elem, to: Elem): Elem
      def trim(isStarting: Boolean): Elem
      def instanceOf(ty: Elem): Elem
      def sizeOf(st: AbsState): Elem

      /** single check */
      def isSingle: Boolean = elem.getSingle match
        case One(_) => true
        case _      => false

      /** get reachable address partitions */
      def reachableParts: Set[Part]

      /** refine receiver object */
      def refineThis(func: Func): Elem

      /** get syntactic SDO */
      def getSdo(method: String): List[(Func, Elem)]

      /** get lexical result */
      def getLexical(method: String): Elem

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
      def ty: ValueTy
      def guard: TypeGuard
    }
  }
}
