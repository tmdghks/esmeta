package esmeta.analyzer.domain.value

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.ir.{COp, Name, VOp, MOp, UOp, Local}
import esmeta.interpreter.Interpreter
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.spec.{Grammar => _, *}
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, Set => MSet}

trait ValueTypeDomainDecl { self: Self =>

  /** type domain for values */
  object ValueTypeDomain extends ValueDomain {

    /** elements */
    case class Elem(
      ty: ValueTy,
      refinements: Refinements = Map(),
    ) extends Appendable

    /** top element */
    lazy val Top: Elem = exploded("top abstract value")

    /** bottom element */
    val Bot: Elem = Elem(ValueTy())

    /** abstraction functions */
    def alpha(vs: Iterable[AValue]): Elem = ??? // Elem(getValueTy(vs))

    /** constructor with types */
    def apply(ty: Ty, refinements: Refinements): Elem = ty match
      case _: UnknownTy => Bot
      case vty: ValueTy => Elem(vty, refinements)

    /** constructor for completions */
    def createCompletion(
      ty: AbsValue,
      value: AbsValue,
      target: AbsValue,
    ): Elem =
      // val enums = ty.ty.enumv
      // val normal =
      //   if (enums contains "normal") value.ty.pureValue
      //   else PureValueTy()
      // val abrupt = enums -- Fin("normal")
      // Elem(ValueTy(normal = normal, abrupt = abrupt))
      ???

    /** predefined top values */
    lazy val cloTop: Elem = Elem(CloT)
    lazy val contTop: Elem = Elem(ContT)
    lazy val partTop: Elem = notSupported("value.TypeDomain.partTop")
    lazy val astValueTop: Elem = Elem(AstT)
    lazy val grammarSymbolTop: Elem = notSupported(
      "value.TypeDomain.grammarSymbolTop",
    )
    lazy val codeUnitTop: Elem = Elem(CodeUnitT)
    lazy val enumTop: Elem = notSupported("value.TypeDomain.enumTop")
    lazy val mathTop: Elem = Elem(MathT)
    lazy val intTop: Elem = Elem(IntT)
    lazy val nonPosIntTop: Elem = Elem(NonPosIntT)
    lazy val nonNegIntTop: Elem = Elem(NonNegIntT)
    lazy val negIntTop: Elem = Elem(NegIntT)
    lazy val posIntTop: Elem = Elem(PosIntT)
    lazy val infinityTop: Elem = Elem(InfinityT)
    lazy val simpleValueTop: Elem =
      notSupported("value.TypeDomain.simpleValueTop")
    lazy val numberTop: Elem = Elem(NumberT)
    lazy val numberIntTop: Elem = Elem(NumberIntT)
    lazy val bigIntTop: Elem = Elem(BigIntT)
    lazy val strTop: Elem = Elem(StrT)
    lazy val boolTop: Elem = Elem(BoolT)
    lazy val undefTop: Elem = Elem(UndefT)
    lazy val nullTop: Elem = Elem(NullT)
    lazy val uninitTop: Elem = ??? // Elem(UninitT)

    /** TODO AST type names whose MV returns a positive integer */
    lazy val posIntMVTyNames: Set[String] = Set(
      "NonZeroDigit",
    )

    /** TODO AST type names whose MV returns a non-negative integer */
    lazy val nonNegIntMVTyNames: Set[String] = Set(
      "CodePoint",
      "Hex4Digits",
      "HexEscapeSequence",
    ) ++ posIntMVTyNames

    /** constructors */
    def apply(
      clo: AbsClo,
      cont: AbsCont,
      part: AbsPart,
      astValue: AbsAstValue,
      grammarSymbol: AbsGrammarSymbol,
      codeUnit: AbsCodeUnit,
      enumv: AbsEnum,
      math: AbsMath,
      infinity: AbsInfinity,
      simpleValue: AbsSimpleValue,
      num: AbsNumber,
      bigInt: AbsBigInt,
      str: AbsStr,
      bool: AbsBool,
      undef: AbsUndef,
      nullv: AbsNull,
    ): Elem = Top

    /** extractors */
    def unapply(elem: Elem): Option[RawTuple] = None

    /** appender */
    given rule: Rule[Elem] = (app, elem) =>
      import TyStringifier.given
      given Rule[Refinements] = sortedMapRule("{", "}", " => ")
      given Rule[Local] = (app, local) => app >> local.toString
      given Rule[Map[Local, ValueTy]] = sortedMapRule("[", "]", " <: ")
      given Rule[RefinementKind] = (app, kind) => app >> kind.toString
      given Ordering[RefinementKind] = Ordering.by(_.toString)
      given Ordering[Local] = Ordering.by(_.toString)
      app >> elem.ty
      if (elem.refinements.nonEmpty) app >> " " >> elem.refinements
      app

    /** transfer for variadic operation */
    def vopTransfer(vop: VOp, vs: List[Elem]): Elem = vop match
      case VOp.Min =>
        val math = vs.map(_.ty.math).reduce(_ min _)
        val inf = vs.map(_.ty.infinity).reduce(_ || _)
        Elem(
          ValueTy(
            math = math,
            infinity = if (math.isBottom) inf else inf && InfinityTy.Neg,
          ),
        )

      case VOp.Max =>
        val math = vs.map(_.ty.math).reduce(_ max _)
        val inf = vs.map(_.ty.infinity).reduce(_ || _)
        Elem(
          ValueTy(
            math = math,
            infinity = if (math.isBottom) inf else inf && InfinityTy.Pos,
          ),
        )
      case VOp.Concat => strTop

    /** transfer for mathematical operation */
    def mopTransfer(mop: MOp, vs: List[Elem]): Elem = mathTop

    /** element interfaces */
    extension (elem: Elem) {

      /** get key values */
      def keyValue: Elem = notSupported("value.TypeDomain.Elem.keyValue")

      /** partial order */
      def ⊑(that: Elem): Boolean = elem.ty <= that.ty

      /** join operator */
      def ⊔(that: Elem): Elem =
        val keys = elem.refinements.keySet ++ that.refinements.keySet
        val refinements = keys.map { key =>
          val lmap = elem.refinements.getOrElse(key, Map.empty)
          val rmap = that.refinements.getOrElse(key, Map.empty)
          val keys = lmap.keySet ++ rmap.keySet
          key -> (keys.map { key =>
            val l = lmap.getOrElse(key, ValueTy.Bot)
            val r = rmap.getOrElse(key, ValueTy.Bot)
            key -> (l || r)
          }.toMap)
        }.toMap
        Elem(elem.ty || that.ty, refinements)

      /** meet operator */
      override def ⊓(that: Elem): Elem =
        val keys = elem.refinements.keySet ++ that.refinements.keySet
        val refinements = keys.map { key =>
          val lmap = elem.refinements.getOrElse(key, Map.empty)
          val rmap = that.refinements.getOrElse(key, Map.empty)
          val keys = lmap.keySet ++ rmap.keySet
          key -> (keys.map { key =>
            val l = lmap.getOrElse(key, ValueTy.Bot)
            val r = rmap.getOrElse(key, ValueTy.Bot)
            key -> (l ⊓ r)
          }.toMap)
        }.toMap
        Elem(elem.ty && that.ty, refinements)

      /** prune operator */
      override def --(that: Elem): Elem = Elem(elem.ty -- that.ty)

      /** concretization function */
      override def gamma: BSet[AValue] = Inf

      /** get single string value */
      override def getSingle: Flat[AValue] = elem.ty.getSingle.map(AValue.from)

      /** get reachable address partitions */
      def reachableParts: Set[Part] = Set()

      /** bitwise operations */
      def &(that: Elem): Elem = ??? // mathOp(elem, that) ⊔ bigIntOp(elem, that)
      def |(that: Elem): Elem = ??? // mathOp(elem, that) ⊔ bigIntOp(elem, that)
      def ^(that: Elem): Elem = ??? // mathOp(elem, that) ⊔ bigIntOp(elem, that)

      /** comparison operations */
      def =^=(that: Elem): Elem =
        (elem.getSingle, that.getSingle) match
          case (Zero, _) | (_, Zero)       => Bot
          case (One(l), One(r))            => Elem(BoolT(l == r))
          case _ if (elem ⊓ that).isBottom => Elem(FalseT)
          case _                           => boolTop
      def ==^==(that: Elem): Elem = ??? // numericCompareOP(elem, that)
      def <(that: Elem): Elem = ??? // numericCompareOP(elem, that)

      /** logical operations */
      def &&(that: Elem): Elem = ??? // logicalOp(_ && _)(elem, that)
      def ||(that: Elem): Elem = ??? // logicalOp(_ || _)(elem, that)
      def ^^(that: Elem): Elem = ??? // logicalOp(_ ^ _)(elem, that)

      /** numeric operations */
      def +(that: Elem): Elem =
        // if (that.ty.math.isPosInt) Elem(ValueTy(math = elem.ty.math match
        //   case m if m.isBottom    => MathTy.Bot
        //   case m if m.isNonNegInt => PosIntTy
        //   case m if m.isInt       => IntTy
        //   case _                  => MathTopTy,
        // ))
        // else numericOp(elem, that)
        ???
      def sub(that: Elem): Elem = ??? // numericOp(elem, that)
      def /(that: Elem): Elem =
        ??? // numericOp(elem, that, intPreserve = ??? // false)
      def *(that: Elem): Elem = ??? // numericOp(elem, that)
      def %(that: Elem): Elem = ??? // numericOp(elem, that)
      def %%(that: Elem): Elem = ??? // numericOp(elem, that)
      def **(that: Elem): Elem = ??? // numericOp(elem, that)
      def <<(that: Elem): Elem =
        ??? // mathOp(elem, that) ⊔ bigIntOp(elem, that)
      def >>(that: Elem): Elem =
        ??? // mathOp(elem, that) ⊔ bigIntOp(elem, that)
      def >>>(that: Elem): Elem = ??? // mathOp(elem, that)

      /** unary negation operation */
      def unary_- : Elem =
        val mathTy = elem.ty.math match
          case MathTopTy      => MathTopTy
          case IntTy          => IntTy
          case NonPosIntTy    => NonNegIntTy
          case NonNegIntTy    => NonPosIntTy
          case PosIntTy       => NegIntTy
          case NegIntTy       => PosIntTy
          case MathSetTy(set) => MathSetTy(set.map(m => Math(-m.decimal)))
        val numberTy = elem.ty.number match
          case NumberTopTy      => NumberTopTy
          case NumberIntTy      => NumberIntTy
          case NumberSetTy(set) => NumberSetTy(set.map(n => Number(-n.double)))
        Elem(ValueTy(math = mathTy, number = numberTy, bigInt = elem.ty.bigInt))

      /** unary logical negation operation */
      def unary_! : Elem = ??? // logicalUnaryOp(!_)(elem)

      /** unary bitwise negation operation */
      def unary_~ : Elem =
        val mathTy = elem.ty.math match
          case MathTopTy | IntTy | NonPosIntTy => IntTy
          case NonNegIntTy | PosIntTy          => NegIntTy
          case NegIntTy                        => PosIntTy
          case MathSetTy(set) =>
            MathSetTy(set.map(m => Math(~(m.decimal.toInt))))
        val numberTy = elem.ty.number match
          case NumberTopTy      => NumberTopTy
          case NumberIntTy      => NumberIntTy
          case NumberSetTy(set) => NumberSetTy(set.filter(_.double.isWhole))
        Elem(ValueTy(math = mathTy, number = numberTy, bigInt = elem.ty.bigInt))

      /** absolute operation */
      def abs: Elem =
        val mathTy = elem.ty.math match
          case MathTopTy                         => MathTopTy
          case IntTy | NonNegIntTy | NonPosIntTy => NonNegIntTy
          case NegIntTy | PosIntTy               => PosIntTy
          case MathSetTy(set) => MathSetTy(set.map(Interpreter.abs))
        Elem(ValueTy(math = mathTy))

      /** floor operation */
      def floor: Elem =
        val mathTy = elem.ty.math match
          case MathTopTy | IntTy                                     => IntTy
          case m @ (NonNegIntTy | NonPosIntTy | NegIntTy | PosIntTy) => m
          case MathSetTy(set) => MathSetTy(set.map(Interpreter.floor))
        Elem(ValueTy(math = mathTy))

      /** type operations */
      def typeOf(st: AbsState): Elem =
        // val ty = elem.ty
        // var names: Set[String] = Set()
        // if (!ty.number.isBottom) names += "Number"
        // if (ty.bigInt) names += "BigInt"
        // if (!ty.str.isBottom) names += "String"
        // if (!ty.bool.isBottom) names += "Boolean"
        // if (ty.undef) names += "Undefined"
        // if (ty.nullv) names += "Null"
        // if (
        //   ty.name.set match
        //     case Inf      => true
        //     case Fin(set) => set.exists(cfg.tyModel.isSubTy(_, "Object"))
        // ) names += "Object"
        // if (
        //   ty.name.set match
        //     case Inf      => true
        //     case Fin(set) => set.exists(cfg.tyModel.isSubTy(_, "Symbol"))
        // ) names += "Symbol"
        // if (
        //   ty.name.set match
        //     case Inf      => true
        //     case Fin(set) => set.exists(!cfg.tyModel.isSubTy(_, "Object"))
        // ) names += "SpecType"
        // Elem(StrT(names))
        ???

      /** type check */
      def typeCheck(tname: String, st: AbsState): Elem =
        // val names = instanceNameSet(elem.ty)
        // if (names.isEmpty) Bot
        // else if (names == Set(tname)) Elem(TrueT)
        // else if (!names.contains(tname)) Elem(FalseT)
        // else boolTop
        ???

      /** helper functions for abstract transfer */
      def convertTo(cop: COp, radix: Elem): Elem =
        val ty = elem.ty
        Elem(cop match
          case COp.ToApproxNumber =>
            if (!ty.math.isBottom) NumberT
            else ValueTy.Bot
          case COp.ToNumber =>
            lazy val fromMath = ty.math match
              case MathTopTy => NumberTopTy
              case MathSetTy(set) =>
                NumberSetTy(set.map(m => Number(m.decimal.toDouble)))
              case _ => NumberIntTy
            if (!ty.str.isBottom) NumberT
            else ValueTy(number = ty.number || fromMath)
          case COp.ToBigInt
              if (!ty.math.isBottom || !ty.str.isBottom || !ty.number.isBottom || ty.bigInt) =>
            BigIntT
          case COp.ToMath =>
            val fromNumber = ty.number match
              case NumberTopTy      => MathTopTy
              case NumberIntTy      => IntTy
              case NumberSetTy(set) => MathSetTy(set.map(n => Math(n.double)))
            val fromBigInt = if (ty.bigInt) IntTy else MathTy.Bot
            ValueTy(math = ty.math || fromNumber || fromBigInt)
          case COp.ToStr(_)
              if (!ty.str.isBottom || !ty.number.isBottom || ty.bigInt) =>
            StrT
          case _ => ValueTy(),
        )
      def sourceText: Elem = strTop
      def parse(rule: Elem): Elem = rule.ty.grammarSymbol match
        case Inf => exploded("too imprecise grammarSymbol rule for parsing")
        case Fin(set) =>
          // Elem(ValueTy(astValue = AstNameTy((for {
          //   grammarSymbol <- set
          //   name = grammarSymbol.name
          // } yield name).toSet)))
          ???
      def substring(from: Elem): Elem = strTop
      def substring(from: Elem, to: Elem): Elem = strTop
      def trim(isStarting: Boolean): Elem = strTop
      def clamp(lower: Elem, upper: Elem): Elem =
        val xty = elem.ty.math
        val lowerTy = lower.ty.math
        val upperTy = upper.ty.math
        val mathTy =
          if (xty.isInt)
            if (lowerTy.isPosInt) PosIntTy
            else if (lowerTy.isNonNegInt) NonNegIntTy
            else if (upperTy.isNegInt) NegIntTy
            else if (upperTy.isNonPosInt) NonPosIntTy
            else IntTy
          else MathTopTy
        Elem(ValueTy(math = mathTy))

      /** completion helpers */
      def wrapCompletion: Elem = ???
      // val ty = elem.ty
      // Elem(
      //   ValueTy(
      //     normal = ty.normal || ty.pureValue,
      //     abrupt = ty.abrupt,
      //   ),
      // )
      def unwrapCompletion: Elem = ???
      // val ty = elem.ty
      // Elem(ValueTy(pureValue = ty.normal || ty.pureValue))
      def isCompletion: Elem = ???
      // val ty = elem.ty
      // var bs: Set[Boolean] = Set()
      // if (!ty.comp.isBottom) bs += true
      // if (!ty.pureValue.isBottom) bs += false
      // Elem(ValueTy(bool = BoolTy(bs)))
      def normalCompletion: Elem = ???
      // Elem(ValueTy(normal = elem.ty.pureValue))
      def abruptCompletion: Elem = ???
      // Elem(ValueTy(abrupt = elem.ty.abrupt))

      /** uninit helpers */
      def removeUnint: Elem = ??? // Elem(elem.ty -- UninitT)
      def isUnint: Elem = ???
      // var bs: Set[Boolean] = Set()
      // if (elem.ty.uninit) bs += true
      // if (!elem.removeUnint.ty.isBottom) bs += false
      // Elem(BoolT(bs))

      /** refine receiver object */
      def refineThis(func: Func): Elem = elem

      /** get lexical result */
      def getLexical(method: String): Elem = ???
      // Elem(
      //   if (elem.ty.astValue.isBottom) ValueTy()
      //   else
      //     method match
      //       case "SV" | "TRV" | "StringValue" => StrT
      //       // TODO handle `list of code points` type
      //       case "IdentifierCodePoints" => StrT
      //       case "MV" =>
      //         elem.ty.astValue.getNames match
      //           case Fin(set) =>
      //             if (set subsetOf posIntMVTyNames) PosIntT
      //             else if (set subsetOf nonNegIntMVTyNames) NonNegIntT
      //             else MathT
      //           case Inf => MathT
      //       case "NumericValue"          => NumericT
      //       case "TV"                    => StrT // XXX ignore UndefT case
      //       case "BodyText" | "FlagText" => StrT
      //       case "Contains"              => BoolT
      //       case _                       => ValueTy(),
      // )

      /** get syntactic SDO */
      def getSdo(method: String): List[(Func, Elem)] =
        // elem.ty.astValue match
        //   case AstTopTy =>
        //     for {
        //       func <- cfg.funcs if func.isSDO
        //       List(_, newMethod) <- allSdoPattern.unapplySeq(func.name)
        //       if newMethod == method
        //     } yield (func, Elem(AstT))
        //   case AstNameTy(names) =>
        //     for {
        //       name <- names.toList
        //       pair <- astSdoCache((name, method))
        //     } yield pair
        //   case AstSingleTy(name, idx, subIdx) =>
        //     synSdoCache((name, idx, subIdx, method))
        ???

      /** getters */
      def clo: AbsClo = ty.clo match
        case Inf => AbsClo.Top
        case Fin(set) =>
          AbsClo(for {
            name <- set.toList
            if cfg.fnameMap.contains(name)
          } yield AClo(cfg.fnameMap(name), Map())) // TODO captured
      def cont: AbsCont = ty.cont match
        case Inf => AbsCont.Top
        case Fin(set) =>
          AbsCont(for {
            fid <- set.toList
            node = cfg.nodeMap(fid)
            func = cfg.funcOf(node)
          } yield ACont(NodePoint(func, node, View()), Map())) // TODO captured
      def part: AbsPart = notSupported("ValueTypeDomain.Elem.part")
      def astValue: AbsAstValue = notSupported("ValueTypeDomain.Elem.astValue")
      def grammarSymbol: AbsGrammarSymbol = notSupported(
        "ValueTypeDomain.Elem.grammarSymbol",
      )
      def codeUnit: AbsCodeUnit = notSupported("ValueTypeDomain.Elem.codeUnit")
      def enumv: AbsEnum = notSupported("ValueTypeDomain.Elem.enumv")
      def math: AbsMath = notSupported("ValueTypeDomain.Elem.math")
      def infinity: AbsInfinity = notSupported("ValueTypeDomain.Elem.infinity")
      def simpleValue: AbsSimpleValue =
        notSupported("ValueTypeDomain.Elem.simpleValue")
      def number: AbsNumber = notSupported("ValueTypeDomain.Elem.number")
      def bigInt: AbsBigInt = notSupported("ValueTypeDomain.Elem.bigInt")
      def str: AbsStr = notSupported("ValueTypeDomain.Elem.str")
      def bool: AbsBool = AbsBool.alpha(elem.ty.bool.set.map(Bool.apply))
      def undef: AbsUndef = notSupported("ValueTypeDomain.Elem.undef")
      def nullv: AbsNull = notSupported("ValueTypeDomain.Elem.nullv")
      def ty: ValueTy = elem.ty
      def refinements: Refinements = elem.refinements
    }

    // -------------------------------------------------------------------------
    // private helpers
    // -------------------------------------------------------------------------
    // // value type getter
    // private def getValueTy(vs: Iterable[AValue]): ValueTy =
    //   vs.foldLeft(ValueTy()) { case (vty, v) => vty || getValueTy(v) }

    // // value type getter
    // private def getValueTy(v: AValue): ValueTy = v match
    //   case AComp(ENUM_NORMAL, v, _) => NormalT(getValueTy(v))
    //   case _: AComp                 => AbruptT
    //   case AClo(func, _)            => CloT(func.name)
    //   case ACont(target, _)         => ContT(target.node.id)
    //   case AstValue(ast)            => AstT(ast.name)
    //   case grammarSymbol: GrammarSymbol                   => GrammarSymbolT(grammarSymbol)
    //   case CodeUnit(_)              => CodeUnitT
    //   case Enum(name)               => EnumT(name)
    //   case Math(n)                  => MathT(n)
    //   case Infinity(pos)            => InfinityT(pos)
    //   case n: Number                => NumberT(n)
    //   case BigInt(_)                => BigIntT
    //   case Str(n)                   => StrT(n)
    //   case Bool(true)               => TrueT
    //   case Bool(false)              => FalseT
    //   case Undef                    => UndefT
    //   case Null                     => NullT
    //   case Uninit                   => UninitT
    //   case v => notSupported(s"impossible to convert to pure type ($v)")

    // // numeric operator helper
    // private def numericOp(l: Elem, r: Elem, intPreserve: Boolean = true) =
    //   mathOp(l, r, intPreserve) ⊔ numberOp(l, r, intPreserve) ⊔ bigIntOp(l, r)

    // // mathematical operator helper
    // private def mathOp(l: Elem, r: Elem, intPreserve: Boolean = true) =
    //   val lty = l.ty.math
    //   val rty = r.ty.math
    //   if (lty.isBottom || rty.isBottom) Bot
    //   else if (intPreserve && lty.isInt && rty.isInt) intTop
    //   else mathTop

    // // number operator helper
    // private def numberOp(l: Elem, r: Elem, intPreserve: Boolean = true) =
    //   val lty = l.ty.number
    //   val rty = r.ty.number
    //   if (lty.isBottom || rty.isBottom) Bot
    //   else if (intPreserve && lty.isInt && rty.isInt) numberIntTop
    //   else numberTop

    // // big integer operator helper
    // private def bigIntOp(l: Elem, r: Elem) =
    //   if (l.ty.bigInt && r.ty.bigInt) bigIntTop
    //   else Bot

    // // logical unary operator helper
    // private def logicalUnaryOp(
    //   op: Boolean => Boolean,
    // ): Elem => Elem =
    //   b => Elem(ValueTy(bool = BoolTy(for (x <- b.ty.bool.set) yield op(x))))

    // // logical operator helper
    // private def logicalOp(
    //   op: (Boolean, Boolean) => Boolean,
    // ): (Elem, Elem) => Elem = (l, r) =>
    //   Elem(ValueTy(bool = BoolTy(for {
    //     x <- l.ty.bool.set
    //     y <- r.ty.bool.set
    //   } yield op(x, y))))

    // // numeric comparison operator helper
    // private lazy val numericCompareOP: (Elem, Elem) => Elem = (l, r) =>
    //   Elem(
    //     ValueTy(
    //       bool = BoolTy(
    //         if (
    //           (
    //             (!l.ty.math.isBottom || !l.ty.number.isBottom) &&
    //             (!r.ty.math.isBottom || !r.ty.number.isBottom)
    //           ) || (l.ty.bigInt && r.ty.bigInt)
    //         ) Set(true, false)
    //         else Set(),
    //       ),
    //     ),
    //   )

    // /** instance name */
    // private def instanceNameSet(ty: ValueTy): Set[String] =
    //   var names: Set[String] = Set()
    //   for (name <- ty.name.set)
    //     names ++= cfg.tyModel.subTys.getOrElse(name, Set(name))
    //     names ++= ancestors(name)
    //   if (!ty.astValue.isBottom) ty.astValue match
    //     case AstTopTy =>
    //       names ++= astChildMap.keySet + "ParseNode" + "Nonterminal"
    //     case ty: AstNonTopTy =>
    //       val astNames = ty.toName.names
    //       names += "ParseNode"
    //       for (astName <- astNames)
    //         names ++= astChildMap.getOrElse(astName, Set(astName))
    //   names

    // /** get ancestor types */
    // private def ancestors(tname: String): Set[String] =
    //   ancestorList(tname).toSet
    // private def ancestorList(tname: String): List[String] =
    //   tname :: parent(tname).map(ancestorList).getOrElse(Nil)

    // /** get parent types */
    // private def parent(name: String): Option[String] = for {
    //   TyDecl(_, parent, _) <- cfg.tyModel.decls.get(name)
    //   p <- parent
    // } yield p

    // /** ast type check helper */
    // lazy val astDirectChildMap: Map[String, Set[String]] =
    //   (cfg.grammar.prods.map {
    //     case Production(lhs, _, _, rhsList) =>
    //       val name = lhs.name
    //       val subs = rhsList.collect {
    //         case Rhs(_, List(Nonterminal(name, _)), _) => name
    //       }.toSet
    //       name -> subs
    //   }).toMap
    // lazy val astChildMap: Map[String, Set[String]] =
    //   var descs = Map[String, Set[String]]()
    //   def aux(name: String): Set[String] = descs.get(name) match
    //     case Some(set) => set
    //     case None =>
    //       val set = (for {
    //         sub <- astDirectChildMap.getOrElse(name, Set())
    //         elem <- aux(sub)
    //       } yield elem) + name
    //       descs += name -> set
    //       set
    //   cfg.grammar.prods.foreach(prod => aux(prod.name))
    //   descs

    // /** sdo access helper */
    // private lazy val astSdoCache: ((String, String)) => List[(Func, Elem)] =
    //   cached[(String, String), List[(Func, Elem)]] {
    //     case (name, method) =>
    //       val result = (for {
    //         (fid, thisTy, hint) <- sdoMap.getOrElse(name, Set())
    //         if hint == method
    //       } yield (cfg.funcMap(fid), Elem(thisTy))).toList
    //       if (result.isEmpty) {
    //         if (defaultSdos contains method) {
    //           val defaultFunc = cfg.fnameMap(s"<DEFAULT>.$method")
    //           for {
    //             (rhs, idx) <- cfg.grammar.nameMap(name).rhsList.zipWithIndex
    //             subIdx <- (0 until rhs.countSubs)
    //           } yield (defaultFunc, Elem(AstSingleT(name, idx, subIdx)))
    //         } else Nil
    //       } else result
    //   }
    // private lazy val synSdoCache =
    //   cached[(String, Int, Int, String), List[(Func, Elem)]] {
    //     case (name, idx, subIdx, method) =>
    //       val result = (for {
    //         (fid, thisTy, hint) <- sdoMap.getOrElse(
    //           s"$name[$idx,$subIdx]",
    //           Set(),
    //         )
    //         if hint == method
    //       } yield (cfg.funcMap(fid), Elem(thisTy))).toList
    //       if (result.isEmpty) {
    //         if (defaultSdos contains method) {
    //           val defaultFunc = cfg.fnameMap(s"<DEFAULT>.$method")
    //           List((defaultFunc, Elem(AstSingleT(name, idx, subIdx))))
    //         } else Nil
    //       } else result
    //   }

    // /** sdo with default case */
    // val defaultSdos = List(
    //   "Contains",
    //   "AllPrivateIdentifiersValid",
    //   "ContainsArguments",
    // )

    // private lazy val allSdoPattern = """(<DEFAULT>|\w+\[\d+,\d+\])\.(\w+)""".r
    // private lazy val sdoPattern = """(\w+)\[(\d+),(\d+)\]\.(\w+)""".r
    // private lazy val sdoMap = {
    //   val edges: MMap[String, MSet[String]] = MMap()
    //   for {
    //     prod <- cfg.grammar.prods
    //     name = prod.name if !(cfg.grammar.lexicalNames contains name)
    //     (rhs, idx) <- prod.rhsList.zipWithIndex
    //     subIdx <- (0 until rhs.countSubs)
    //   } {
    //     val syntacticName = s"$name[$idx,$subIdx]"
    //     edges += (syntacticName -> MSet(name))
    //     rhs.getGrammarSymbols(subIdx) match
    //       case List(Some(chain)) =>
    //         if (edges contains chain) edges(chain) += syntacticName
    //         else edges(chain) = MSet(syntacticName)
    //       case _ =>
    //   }
    //   val worklist = QueueWorklist[String](List())
    //   val map: MMap[String, MSet[(Int, ValueTy, String)]] = MMap()
    //   var defaultmap: MMap[String, MSet[(Int, ValueTy, String)]] = MMap()
    //   for {
    //     func <- cfg.funcs if func.isSDO
    //     isDefaultSdo = func.name.startsWith("<DEFAULT>") if !isDefaultSdo
    //     List(name, idxStr, subIdxStr, method) <- sdoPattern.unapplySeq(
    //       func.name,
    //     )
    //     (idx, subIdx) = (idxStr.toInt, subIdxStr.toInt)
    //     key = s"$name[$idx,$subIdx]"
    //   } {
    //     val newInfo = (func.id, AstSingleT(name, idx, subIdx), method)
    //     val isDefaultSdo = defaultSdos contains method

    //     // update target info
    //     val targetmap = if (isDefaultSdo) defaultmap else map
    //     if (targetmap contains key) targetmap(key) += newInfo
    //     else targetmap(key) = MSet(newInfo)
    //     if (targetmap contains name) targetmap(name) += newInfo
    //     else targetmap(name) = MSet(newInfo)

    //   // record original map
    //   val origmap = (for { (k, set) <- map } yield k -> set.toSet).toMap

    //   // propagate chain productions
    //   @tailrec
    //   def aux(): Unit = worklist.next match
    //     case Some(key) =>
    //       val childInfo = map.getOrElse(key, MSet())
    //       for {
    //         next <- edges.getOrElse(key, MSet())
    //         info = map.getOrElse(next, MSet())
    //         oldmapize = info.size

    //         newInfo =
    //           // A[i,j] -> A
    //           if (key endsWith "]") info ++ childInfo
    //           // A.method -> B[i,j].method
    //           // only if B[i,j].method not exists (chain production)
    //           else {
    //             val origInfo = origmap.getOrElse(next, Set())
    //             info ++ (for {
    //               triple <- childInfo
    //               if !(origInfo.exists(_._3 == triple._3))
    //             } yield triple)
    //           }

    //         _ = map(next) = newInfo
    //         if newInfo.size > oldmapize
    //       } worklist += next
    //       aux()
    //     case None => /* do nothing */
    //   aux()

    //   // merge default map
    //   (for {
    //     key <- map.keySet ++ defaultmap.keySet
    //     info = map.getOrElse(key, MSet())
    //     defaultInfo = defaultmap.getOrElse(key, MSet())
    //     finalInfo = (info ++ defaultInfo).toSet
    //   } yield key -> finalInfo).toMap
    // }
  }
}
