package esmeta.analyzer.domain.pureValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*

trait PureValueBasicDomainDecl { self: Self =>

  /** basic domain for pure values */
  object PureValueBasicDomain extends PureValueDomain {

    /** elements */
    case class Elem(
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
    ) extends Appendable

    /** top element */
    lazy val Top: Elem = exploded("top abstract pure value")

    /** bottom element */
    val Bot: Elem = Elem()

    /** abstraction functions */
    def alpha(xs: Iterable[APureValue]): Elem = Elem(
      AbsClo(xs.collect { case x: AClo => x }),
      AbsCont(xs.collect { case x: ACont => x }),
      AbsPart(xs.collect { case x: Part => x }),
      AbsAstValue(xs.collect { case x: AstValue => x }),
      AbsGrammarSymbol(xs.collect { case x: GrammarSymbol => x }),
      AbsCodeUnit(xs.collect { case x: CodeUnit => x }),
      AbsEnum(xs.collect { case x: Enum => x }),
      AbsMath(xs.collect { case x: Math => x }),
      AbsInfinity(xs.collect { case x: Infinity => x }),
      AbsSimpleValue(xs.collect { case x: SimpleValue => x }),
    )

    /** predefined top values */
    val cloTop: Elem = Bot.copy(clo = AbsClo.Top)
    val contTop: Elem = Bot.copy(cont = AbsCont.Top)
    val partTop: Elem = Bot.copy(part = AbsPart.Top)
    val astValueTop: Elem = Bot.copy(astValue = AbsAstValue.Top)
    val grammarSymbolTop: Elem = Bot.copy(grammarSymbol = AbsGrammarSymbol.Top)
    val codeUnitTop: Elem = Bot.copy(codeUnit = AbsCodeUnit.Top)
    val enumTop: Elem = Bot.copy(enumv = AbsEnum.Top)
    val mathTop: Elem = Bot.copy(math = AbsMath.Top)
    val infinityTop: Elem = Bot.copy(infinity = AbsInfinity.Top)
    val simpleValueTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.Top)
    val numberTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.numberTop)
    val bigIntTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.bigIntTop)
    val strTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.strTop)
    val boolTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.boolTop)
    val undefTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.undefTop)
    val nullTop: Elem = Bot.copy(simpleValue = AbsSimpleValue.nullTop)

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
    ): Elem = Elem(
      clo,
      cont,
      part,
      astValue,
      grammarSymbol,
      codeUnit,
      enumv,
      math,
      infinity,
      simpleValue ⊔ AbsSimpleValue(num, bigInt, str, bool, undef, nullv),
    )

    /** extractors */
    def unapply(elem: Elem): Option[RawTuple] = Some(
      (
        elem.clo,
        elem.cont,
        elem.part,
        elem.astValue,
        elem.grammarSymbol,
        elem.codeUnit,
        elem.enumv,
        elem.math,
        elem.infinity,
        elem.simpleValue,
      ),
    )

    /** appender */
    given rule: Rule[Elem] = (app, elem) => {
      if (elem.isBottom) app >> "⊥"
      else {
        var strs = Vector[String]()
        if (!elem.clo.isBottom) strs :+= elem.clo.toString
        if (!elem.cont.isBottom) strs :+= elem.cont.toString
        if (!elem.part.isBottom) strs :+= elem.part.toString
        if (!elem.astValue.isBottom) strs :+= elem.astValue.toString
        if (!elem.grammarSymbol.isBottom) strs :+= elem.grammarSymbol.toString
        if (!elem.codeUnit.isBottom) strs :+= elem.codeUnit.toString
        if (!elem.enumv.isBottom) strs :+= elem.enumv.toString
        if (!elem.math.isBottom) strs :+= elem.math.toString
        if (!elem.infinity.isBottom) strs :+= elem.infinity.toString
        if (!elem.simpleValue.isBottom) strs :+= elem.simpleValue.toString
        app >> strs.mkString(", ")
      }
    }

    /** element interfaces */
    extension (elem: Elem) {

      /** partial order */
      def ⊑(that: Elem): Boolean =
        elem.clo ⊑ that.clo &&
        elem.cont ⊑ that.cont &&
        elem.part ⊑ that.part &&
        elem.astValue ⊑ that.astValue &&
        elem.grammarSymbol ⊑ that.grammarSymbol &&
        elem.codeUnit ⊑ that.codeUnit &&
        elem.enumv ⊑ that.enumv &&
        elem.math ⊑ that.math &&
        elem.infinity ⊑ that.infinity &&
        elem.simpleValue ⊑ that.simpleValue

      /** join operator */
      def ⊔(that: Elem): Elem = Elem(
        elem.clo ⊔ that.clo,
        elem.cont ⊔ that.cont,
        elem.part ⊔ that.part,
        elem.astValue ⊔ that.astValue,
        elem.grammarSymbol ⊔ that.grammarSymbol,
        elem.codeUnit ⊔ that.codeUnit,
        elem.enumv ⊔ that.enumv,
        elem.math ⊔ that.math,
        elem.infinity ⊔ that.infinity,
        elem.simpleValue ⊔ that.simpleValue,
      )

      /** meet operator */
      override def ⊓(that: Elem): Elem = Elem(
        elem.clo ⊓ that.clo,
        elem.cont ⊓ that.cont,
        elem.part ⊓ that.part,
        elem.astValue ⊓ that.astValue,
        elem.grammarSymbol ⊓ that.grammarSymbol,
        elem.codeUnit ⊓ that.codeUnit,
        elem.enumv ⊓ that.enumv,
        elem.math ⊓ that.math,
        elem.infinity ⊓ that.infinity,
        elem.simpleValue ⊓ that.simpleValue,
      )

      /** prune operator */
      override def --(that: Elem): Elem = Elem(
        elem.clo -- that.clo,
        elem.cont -- that.cont,
        elem.part -- that.part,
        elem.astValue -- that.astValue,
        elem.grammarSymbol -- that.grammarSymbol,
        elem.codeUnit -- that.codeUnit,
        elem.enumv -- that.enumv,
        elem.math -- that.math,
        elem.infinity -- that.infinity,
        elem.simpleValue -- that.simpleValue,
      )

      /** concretization function */
      override def gamma: BSet[APureValue] =
        (elem.clo.gamma: BSet[APureValue]) ⊔
        elem.cont.gamma ⊔
        elem.part.gamma ⊔
        elem.astValue.gamma ⊔
        elem.grammarSymbol.gamma ⊔
        elem.codeUnit.gamma ⊔
        elem.enumv.gamma ⊔
        elem.math.gamma ⊔
        elem.infinity.gamma ⊔
        elem.simpleValue.gamma

      /** get single value */
      override def getSingle: Flat[APureValue] =
        (elem.clo.getSingle: Flat[APureValue]) ⊔
        elem.cont.getSingle ⊔
        elem.part.getSingle ⊔
        elem.astValue.getSingle ⊔
        elem.grammarSymbol.getSingle ⊔
        elem.codeUnit.getSingle ⊔
        elem.enumv.getSingle ⊔
        elem.math.getSingle ⊔
        elem.infinity.getSingle ⊔
        elem.simpleValue.getSingle

      /** getters */
      def clo: AbsClo = elem.clo
      def cont: AbsCont = elem.cont
      def part: AbsPart = elem.part
      def astValue: AbsAstValue = elem.astValue
      def grammarSymbol: AbsGrammarSymbol = elem.grammarSymbol
      def codeUnit: AbsCodeUnit = elem.codeUnit
      def enumv: AbsEnum = elem.enumv
      def math: AbsMath = elem.math
      def infinity: AbsInfinity = elem.infinity
      def simpleValue: AbsSimpleValue = elem.simpleValue
      def number: AbsNumber = elem.simpleValue.number
      def bigInt: AbsBigInt = elem.simpleValue.bigInt
      def str: AbsStr = elem.simpleValue.str
      def bool: AbsBool = elem.simpleValue.bool
      def undef: AbsUndef = elem.simpleValue.undef
      def nullv: AbsNull = elem.simpleValue.nullv

      /** get reachable address partitions */
      def reachableParts: Set[Part] =
        var parts = part.toSet
        for {
          AClo(_, captured) <- clo
          (_, value) <- captured
        } parts ++= value.reachableParts
        for {
          ACont(_, captured) <- cont
          (_, value) <- captured
        } parts ++= value.reachableParts
        parts
    }
  }
}
