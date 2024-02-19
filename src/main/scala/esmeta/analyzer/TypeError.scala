package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.util.*

trait TypeErrorDecl { self: Analyzer =>

  /** type errors in specification */
  sealed trait TypeError extends AnalyzerElem {
    val point: TypeErrorPoint
    inline def func: Func = point.func
  }

  /** parameter type mismatches */
  case class ParamTypeMismatch(
    point: ArgAssignPoint,
    argTy: ValueTy,
  ) extends TypeError

  /** return type mismatches */
  case class ReturnTypeMismatch(
    point: InternalReturnPoint,
    retTy: ValueTy,
  ) extends TypeError

  /** arity mismatches */
  case class ArityMismatch(
    point: CallPoint,
    actual: Int,
  ) extends TypeError

  /** unchecked abrupt completion errors */
  case class UncheckedAbruptError(
    point: ReturnIfAbruptPoint,
    ty: ValueTy,
  ) extends TypeError

  /** invalid base in property reference errors */
  case class InvalidBaseError(
    point: PropBasePoint,
    baseTy: ValueTy,
  ) extends TypeError

  /** operand type mismatches for unary operators */
  case class UnaryOpTypeMismatch(
    point: UnaryOpPoint,
    operandTy: ValueTy,
  ) extends TypeError

  /** operand type mismatches for binary operators */
  case class BinaryOpTypeMismatch(
    point: BinaryOpPoint,
    lhsTy: ValueTy,
    rhsTy: ValueTy,
  ) extends TypeError
}
