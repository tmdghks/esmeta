package esmeta.ty

import esmeta.cfg.Node
import esmeta.state.*
import esmeta.ty.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** type elements */
trait TyElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}

// -----------------------------------------------------------------------------
// helpers
// -----------------------------------------------------------------------------
def May(ty: ValueTy): OptValueTy = OptValueTy(ty, true)
def Must(ty: ValueTy): OptValueTy = OptValueTy(ty, false)
lazy val MayAnyT: OptValueTy = OptValueTy(AnyT, true)
lazy val MustBotT: OptValueTy = OptValueTy(BotT, false)
lazy val AnyT: ValueTy = ValueTy.Top
lazy val CompT: ValueTy = ValueTy(record = RecordTy("Completion"))
lazy val AbruptT: ValueTy = ValueTy(record = RecordTy("Abrupt"))
def AbruptT(names: String*): ValueTy = AbruptT(names.toSet)
def AbruptT(names: Set[String]): ValueTy =
  ValueTy(record = RecordTy("Abrupt", Map("Type" -> EnumT(names.toSet))))
lazy val NormalT: ValueTy = ValueTy(record = RecordTy("Normal"))
def NormalT(value: ValueTy): ValueTy =
  if (value.isBottom) BotT
  else ValueTy(record = RecordTy("Normal", Map("Value" -> value)))
def MapT: ValueTy = ValueTy(map = MapTy.Top)
def MapT(key: ValueTy, value: ValueTy): ValueTy =
  if (key.isBottom || value.isBottom) BotT
  else ValueTy(map = MapTy(key, value))
lazy val CloT: ValueTy = ValueTy(clo = Inf)
def CloT(names: String*): ValueTy =
  if (names.isEmpty) BotT
  else ValueTy(clo = Fin(names.toSet))
lazy val ContT: ValueTy = ValueTy(cont = Inf)
def ContT(nids: Int*): ValueTy =
  if (nids.isEmpty) BotT
  else ValueTy(cont = Fin(nids.toSet))
lazy val ObjectT: ValueTy = RecordT("Object")
lazy val FunctionT: ValueTy = RecordT("FunctionObject")
lazy val ConstructorT: ValueTy = RecordT("Constructor")
lazy val ESPrimT: ValueTy = ValueTy(
  record = RecordTy("Symbol"),
  number = NumberTy.Top,
  bigInt = true,
  str = Inf,
  bool = BoolTy.Top,
  undef = true,
  nullv = true,
)
lazy val ESValueT: ValueTy = ObjectT || ESPrimT
lazy val RecordT: ValueTy = ValueTy(record = RecordTy.Top)
def RecordT(names: Set[String]): ValueTy = ValueTy(record = RecordTy(names))
def RecordT(names: String*): ValueTy = RecordT(names.toSet)
def RecordT(name: String, fields: Map[String, ValueTy]): ValueTy =
  ValueTy(record = RecordTy(name, fields))
def NilT: ValueTy = ValueTy(list = ListTy.Nil)
def ListT: ValueTy = ValueTy(list = ListTy.Top)
def ListT(ty: ValueTy): ValueTy = ValueTy(list = ListTy(ty))
lazy val SymbolT: ValueTy = RecordT("Symbol")
lazy val AstT: ValueTy = ValueTy(ast = AstTy.Top)
def AstT(xs: Set[String]): ValueTy =
  if (xs.isEmpty) BotT
  else ValueTy(ast = AstTy.Simple(xs.toSet))
def AstT(xs: String*): ValueTy = AstT(xs.toSet)
def AstT(name: String, idx: Int): ValueTy =
  ValueTy(ast = AstTy.Detail(name, idx))
def GrammarSymbolT: ValueTy = ValueTy(grammarSymbol = Inf)
def GrammarSymbolT(xs: GrammarSymbol*): ValueTy =
  if (xs.isEmpty) BotT
  else ValueTy(grammarSymbol = Fin(xs.toSet))
lazy val CodeUnitT: ValueTy = ValueTy(codeUnit = true)
def EnumT: ValueTy = ValueTy(enumv = Inf)
def EnumT(set: Set[String]): ValueTy =
  if (set.isEmpty) BotT
  else ValueTy(enumv = Fin(set))
def EnumT(xs: String*): ValueTy = EnumT(xs.toSet)
lazy val MathT: ValueTy = ValueTy(math = MathTy.Top)
lazy val ExtMathT: ValueTy = MathT || InfinityT
lazy val IntT: ValueTy = ValueTy(math = IntTy)
lazy val NonPosIntT: ValueTy = ValueTy(math = NonPosIntTy)
lazy val NonNegIntT: ValueTy = ValueTy(math = NonNegIntTy)
lazy val NegIntT: ValueTy = ValueTy(math = NegIntTy)
lazy val PosIntT: ValueTy = ValueTy(math = PosIntTy)
def MathT(ds: BigDecimal*): ValueTy =
  if (ds.isEmpty) BotT
  else ValueTy(math = MathSetTy(ds.toSet.map(Math(_))))
lazy val InfinityT: ValueTy = ValueTy(infinity = InfinityTy.Top)
lazy val NegInfinityT: ValueTy = ValueTy(infinity = InfinityTy.Neg)
lazy val PosInfinityT: ValueTy = ValueTy(infinity = InfinityTy.Pos)
def InfinityT(ps: Boolean*): ValueTy =
  if (ps.isEmpty) BotT
  else ValueTy(infinity = InfinityTy(ps.toSet))
lazy val NumericT: ValueTy = NumberT || BigIntT
lazy val NumberT: ValueTy = ValueTy(number = NumberTy.Top)
lazy val NumberIntT: ValueTy = ValueTy(number = NumberTy.Int)
def NumberT(ns: Number*): ValueTy =
  if (ns.isEmpty) BotT
  else ValueTy(number = NumberSetTy(ns.toSet))
lazy val BigIntT: ValueTy = ValueTy(bigInt = true)
lazy val StrT: ValueTy = ValueTy(str = Inf)
def StrT(set: Set[String]): ValueTy =
  if (set.isEmpty) BotT
  else ValueTy(str = Fin(set))
def StrT(xs: String*): ValueTy =
  if (xs.isEmpty) BotT
  else ValueTy(str = Fin(xs.toSet))
def BoolT(set: Set[Boolean]): ValueTy =
  if (set.isEmpty) BotT
  else ValueTy(bool = BoolTy(set))
def BoolT(seq: Boolean*): ValueTy =
  if (seq.isEmpty) BotT
  else ValueTy(bool = BoolTy(seq.toSet))
lazy val BoolT: ValueTy = BoolT(true, false)
lazy val TrueT: ValueTy = BoolT(true)
lazy val FalseT: ValueTy = BoolT(false)
lazy val UndefT: ValueTy = ValueTy(undef = true)
lazy val NullT: ValueTy = ValueTy(nullv = true)
lazy val BotT: ValueTy = ValueTy.Bot

/** predefined enum types */
val ENUMT_EMPTY = EnumT("empty")
val ENUMT_UNRESOLVABLE = EnumT("unresolvable")
val ENUMT_LEXICAL = EnumT("lexical")
val ENUMT_INITIALIZED = EnumT("initialized")
val ENUMT_UNINITIALIZED = EnumT("uninitialized")
val ENUMT_BASE = EnumT("base")
val ENUMT_DERIVED = EnumT("derived")
val ENUMT_STRICT = EnumT("strict")
val ENUMT_GLOBAL = EnumT("global")
val ENUMT_UNLINKED = EnumT("unlinked")
val ENUMT_LINKING = EnumT("linking")
val ENUMT_LINKED = EnumT("linked")
val ENUMT_EVALUATING = EnumT("evaluating")
val ENUMT_EVALUATED = EnumT("evaluated")
val ENUMT_NUMBER = EnumT("Number")
val ENUMT_BIGINT = EnumT("BigInt")
val ENUMT_NORMAL = EnumT("normal")
val ENUMT_BREAK = EnumT("break")
val ENUMT_CONTINUE = EnumT("continue")
val ENUMT_RETURN = EnumT("return")
val ENUMT_THROW = EnumT("throw")
val ENUMT_SUSPENDED_START = EnumT("suspendedStart")
val ENUMT_SUSPENDED_YIELD = EnumT("suspendedYield")
val ENUMT_EXECUTING = EnumT("executing")
val ENUMT_AWAITING_RETURN = EnumT("awaitingDASHreturn")
val ENUMT_COMPLETED = EnumT("completed")
val ENUMT_PENDING = EnumT("pending")
val ENUMT_FULFILLED = EnumT("fulfilled")
val ENUMT_REJECTED = EnumT("rejected")
val ENUMT_FULFILL = EnumT("Fulfill")
val ENUMT_REJECT = EnumT("Reject")

/** method map */
type MethodMap = Map[String, String]

extension (elem: Boolean) {
  inline def isTop: Boolean = elem == true
  inline def isBottom: Boolean = elem == false
  inline def --(that: Boolean): Boolean = elem && !that
}
