package esmeta.state

import esmeta.cfg.Func
import esmeta.error.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.DoubleEquals
import java.math.MathContext.UNLIMITED
import scala.collection.mutable.{Map => MMap}

/** IR values */
sealed trait Value extends StateElem {

  /** check abrupt completion */
  def isCompletion: Boolean = this match
    case comp: Comp => true
    case _          => false

  /** check abrupt completion */
  def isAbruptCompletion: Boolean = this match
    case comp: Comp => comp.ty != ENUM_NORMAL
    case _          => false

  /** wrap completion */
  def wrapCompletion: Comp = wrapCompletion(ENUM_NORMAL)
  def wrapCompletion(ty: Enum): Comp = this match
    case absent: Absent  => throw UncheckedAbsent
    case comp: Comp      => comp
    case pure: PureValue => Comp(ty, pure, None)

  /** convert value to pure value see:
    * https://github.com/es-meta/esmeta/issues/66
    */
  def toPureValue: PureValue = this match
    case Absent          => throw UncheckedAbsent
    case comp: Comp      => throw UncheckedAbrupt(comp)
    case pure: PureValue => pure

  /** type conversion */
  def asStr: String = this match
    case Str(s)      => s
    case CodeUnit(c) => c.toString
    case _           => throw NotStringType(this)
  def asInt: Int = this match
    case Number(n) if n.isValidInt => n.toInt
    case Math(n) if n.isValidInt   => n.toInt
    case _                         => throw NotIntType(this)
  def asAst: Ast = this match
    case AstValue(ast) => ast
    case v             => throw NotAstType(this)
  def asMath: BigDecimal = this match
    case Math(n) => n
    case v       => throw NotDecimalType(this)
  def getList(e: Expr, st: State): ListObj = this match
    case addr: Addr =>
      st(addr) match
        case l: ListObj => l
        case obj        => throw NoList(e, obj)
    case _ => throw NoAddr(e, this)
}

/** absent values to represent missing references */
case object Absent extends Value

/** completion values */
case class Comp(
  ty: Enum,
  var value: PureValue, // XXX YieldExpression[2,0].Evaluation
  target: Option[String],
) extends Value {
  def targetValue: PureValue = target.fold[PureValue](ENUM_EMPTY)(Str(_))
}

/** normal completion */
object NormalComp {
  def apply(value: Value): Comp =
    Comp(ENUM_NORMAL, value.toPureValue, None)
  def unapply(comp: Comp): Option[PureValue] = comp match {
    case Comp(ENUM_NORMAL, value, None) => Some(value)
    case _                              => None
  }
}

/** pure values (values except completion records) */
sealed trait PureValue extends Value

/** addresses */
sealed trait Addr extends PureValue
case class NamedAddr(name: String) extends Addr
case class DynamicAddr(long: Long) extends Addr

/** ordering of addresses */
given Ordering[Addr] = Ordering.by(_ match
  case NamedAddr(name)   => (-1L, name)
  case DynamicAddr(long) => (long, ""),
)

/** function values */
sealed trait FuncValue extends PureValue {
  def func: Func
  def captured: Map[Name, Value]
}

/** closures */
case class Clo(func: Func, captured: Map[Name, Value]) extends FuncValue

/** continuations */
case class Cont(
  func: Func,
  captured: Map[Name, Value],
  callStack: List[CallContext],
) extends FuncValue

/** abstract syntax tree (AST) values */
case class AstValue(ast: Ast) extends PureValue

/** grammar symbols */
case class GrammarSymbol(name: String, params: List[Boolean]) extends PureValue

/** mathematical values */
case class Math(decimal: BigDecimal) extends PureValue
object Math {
  val zero: Math = Math(0)
  val one: Math = Math(1)
  inline def apply(n: Int): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: Long): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: Double): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: scala.math.BigInt): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(s: String): Math = Math(BigDecimal(s, UNLIMITED))
  inline def from(s: String, b: Int): Math = apply(scala.math.BigInt(s, b))
  inline def fromBinary(s: String): Math = from(s, 2)
  inline def fromOctal(s: String): Math = from(s, 8)
  inline def fromHex(s: String): Math = from(s, 16)

  extension (m: Math) {
    def +(n: Math): Math = Math(m.decimal + n.decimal)
    def -(n: Math): Math = Math(m.decimal - n.decimal)
    def *(n: Math): Math = Math(m.decimal * n.decimal)
    def /(n: Math): Math = Math(m.decimal / n.decimal)
    def pow(n: Math): Math = Math(m.decimal.pow(n.toInt))
    def unary_- : Math = Math(-m.decimal)
    def toInt: Int = m.decimal.toInt
    def toLong: Long = m.decimal.toLong
    def toDouble: Double = m.decimal.toDouble
    def toBigInt: BigInt = BigInt(m.decimal.toBigInt)
    def toBigDecimal: BigDecimal = m.decimal
  }
}

/** infinity values */
case class Infinity(pos: Boolean) extends PureValue

/** enums */
case class Enum(name: String) extends PureValue

/** code units */
case class CodeUnit(c: Char) extends PureValue

/** simple values
  *
  * Simple values are ECMAScript values except objects and symbols. ECMAScript
  * objects and symbols need to be stored in a heap.
  */
sealed trait SimpleValue extends PureValue

/** numeric values */
sealed trait Numeric extends SimpleValue:
  def toMath: Math = this match
    case Number(double) => Math(double)
    case BigInt(bigInt) => Math(bigInt)
case class Number(double: Double) extends Numeric with DoubleEquals
case class BigInt(bigInt: scala.math.BigInt) extends Numeric

/** non-numeric simple values */
case class Str(str: String) extends SimpleValue
case class Bool(bool: Boolean) extends SimpleValue
case object Undef extends SimpleValue
case object Null extends SimpleValue
