package esmeta.ir

import esmeta.util.BaseUtils._
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

/** IR Programs */
case class Program(insts: List[Inst]) extends IRElem
object Program extends Parser[Program]

/** IR Instructions */
enum Inst extends IRElem:
  case IIf(cond: Expr, thenInst: Inst, elseInst: Inst)
  case IWhile(cond: Expr, body: Inst)
  case IApp(id: Id, fexpr: Expr, args: List[Expr])
  case IAccess(id: Id, bexpr: Expr, expr: Expr, args: List[Expr])
  case IExpr(expr: Expr)
  case ILet(id: Id, expr: Expr)
  case IAssign(ref: Ref, expr: Expr)
  case IDelete(ref: Ref)
  case IAppend(expr: Expr, list: Expr)
  case IPrepend(expr: Expr, list: Expr)
  case IReturn(expr: Expr)
  case IThrow(name: String)
  case IAssert(expr: Expr)
  case IPrint(expr: Expr)
  case IClo(id: Id, params: List[Id], captured: List[Id], body: Inst)
  case ICont(id: Id, params: List[Id], body: Inst)
  case IWithCont(id: Id, params: List[Id], body: Inst)
  case ISeq(insts: List[Inst])
object Insts extends Parser[List[Inst]]
object Inst extends Parser[Inst] {
  // specific categories of intsructions
  type CondInst = IIf | IWhile
  type CallInst = IApp | IAccess
  type ArrowInst = IClo | ICont | IWithCont
  type NormalInst = IExpr | ILet | IAssign | IDelete | IAppend | IPrepend |
    IReturn | IThrow | IAssert | IPrint
}

/** IR Expressions */
enum Expr extends IRElem:
  case ENum(n: Double)
  case EINum(n: Long)
  case EBigINum(b: BigInt)
  case EStr(str: String)
  case EBool(b: Boolean)
  case EUndef
  case ENull
  case EAbsent
  case EConst(name: String)
  case EComp(ty: Expr, value: Expr, target: Expr)
  case EMap(ty: Ty, props: List[(Expr, Expr)])
  case EList(exprs: List[Expr])
  case ESymbol(desc: Expr)
  case EPop(list: Expr, idx: Expr)
  case ERef(ref: Ref)
  case EUOp(uop: UOp, expr: Expr)
  case EBOp(bop: BOp, left: Expr, right: Expr)
  case ETypeOf(expr: Expr)
  case EIsCompletion(expr: Expr)
  case EIsInstanceOf(base: Expr, name: String)
  case EGetElems(base: Expr, name: String)
  case EGetSyntax(base: Expr)
  case EParseSyntax(code: Expr, rule: Expr, parserParams: List[Boolean])
  case EConvert(source: Expr, target: COp, flags: List[Expr])
  case EContains(list: Expr, elem: Expr)
  case EReturnIfAbrupt(expr: Expr, check: Boolean)
  case ECopy(obj: Expr)
  case EKeys(mobj: Expr, intSorted: Boolean)
  case ENotSupported(msg: String)
object Expr extends Parser[Expr]

/** IR References */
enum Ref extends IRElem:
  case RefId(id: Id)
  case RefProp(ref: Ref, expr: Expr)
object Ref extends Parser[Ref]

/** IR Identifiers */
case class Id(name: String) extends IRElem
object Id extends Parser[Id]

/** IR Types */
case class Ty(name: String) extends IRElem
object Ty extends Parser[Ty]

/** IR Unary Operators */
enum UOp extends IRElem:
  case ONeg, ONot, OBNot
object UOp extends Parser[UOp]

/** IR Binary Operators */
enum BOp extends IRElem:
  case OPlus
  case OSub
  case OMul
  case OPow
  case ODiv
  case OUMod
  case OMod
  case OLt
  case OEq
  case OEqual
  case OAnd
  case OOr
  case OXor
  case OBAnd
  case OBOr
  case OBXOr
  case OLShift
  case OSRShift
  case OURShift
object BOp extends Parser[BOp]

/** IR Convert Operators */
enum COp extends IRElem:
  case CStrToNum
  case CStrToBigInt
  case CNumToStr
  case CNumToInt
  case CNumToBigInt
  case CBigIntToNum
object COp extends Parser[COp]