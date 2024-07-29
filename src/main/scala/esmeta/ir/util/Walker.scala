package esmeta.ir.util

import esmeta.util.BasicWalker
import esmeta.ir.*

/** a walker for IR */
trait Walker extends BasicWalker {
  def walk(elem: IRElem): IRElem = elem match
    case elem: Program  => walk(elem)
    case elem: Func     => walk(elem)
    case elem: FuncKind => walk(elem)
    case elem: Param    => walk(elem)
    case elem: Inst     => walk(elem)
    case elem: Expr     => walk(elem)
    case elem: UOp      => walk(elem)
    case elem: BOp      => walk(elem)
    case elem: VOp      => walk(elem)
    case elem: MOp      => walk(elem)
    case elem: COp      => walk(elem)
    case elem: Ref      => walk(elem)
    case elem: Type     => walk(elem)

  // programs
  def walk(program: Program): Program =
    Program(walkList(program.funcs, walk), program.spec)

  // functions
  def walk(func: Func): Func =
    val Func(main, kind, name, ps, rty, body, algo) = func
    Func(
      walk(main),
      walk(kind),
      walk(name),
      walkList(ps, walk),
      walk(rty),
      walk(func.body),
      algo,
    )

  // function kinds
  def walk(kind: FuncKind): FuncKind = kind

  // function parameters
  def walk(param: Param): Param =
    val Param(name, opt, ty, specParam) = param
    Param(walk(name), walk(opt), walk(ty), specParam)

  // instructions
  def walk(inst: Inst): Inst = (inst match
    case IExpr(expr)            => IExpr(walk(expr))
    case ILet(lhs, expr)        => ILet(walk(lhs), walk(expr))
    case IAssign(ref, expr)     => IAssign(walk(ref), walk(expr))
    case IDelete(ref)           => IDelete(walk(ref))
    case IPush(from, to, front) => IPush(walk(from), walk(to), walk(front))
    case IReturn(expr)          => IReturn(walk(expr))
    case IAssert(expr)          => IAssert(walk(expr))
    case IPrint(expr)           => IPrint(walk(expr))
    case INop()                 => INop()
    case ISeq(insts)            => ISeq(walkList(insts, walk))
    case IIf(c, t, e)           => IIf(walk(c), walk(t), walk(e))
    case IWhile(c, b)           => IWhile(walk(c), walk(b))
    case ICall(l, f, as)        => ICall(walk(l), walk(f), walkList(as, walk))
    case ISdoCall(l, b, n, as) =>
      ISdoCall(walk(l), walk(b), walk(n), walkList(as, walk))
  ).setLangOpt(inst.langOpt)

  // expressions
  def walk(expr: Expr): Expr = (expr match
    case EComp(tyExpr, valExpr, tgtExpr) =>
      EComp(walk(tyExpr), walk(valExpr), walk(tgtExpr))
    case EIsCompletion(expr) =>
      EIsCompletion(walk(expr))
    case EReturnIfAbrupt(expr, check) =>
      EReturnIfAbrupt(walk(expr), walk(check))
    case EPop(list, front) =>
      EPop(walk(list), walk(front))
    case EParse(code, rule) =>
      EParse(walk(code), walk(rule))
    case EGrammarSymbol(name, params) =>
      EGrammarSymbol(walk(name), walkList(params, walk))
    case ESourceText(expr) =>
      ESourceText(walk(expr))
    case EYet(msg) =>
      EYet(walk(msg))
    case EContains(list, elem) =>
      EContains(walk(list), walk(elem))
    case ESubstring(expr, from, to) =>
      ESubstring(walk(expr), walk(from), walkOpt(to, walk))
    case ETrim(expr, isStarting) =>
      ETrim(walk(expr), walk(isStarting))
    case ERef(ref) =>
      ERef(walk(ref))
    case EUnary(uop, expr) =>
      EUnary(walk(uop), walk(expr))
    case EBinary(bop, left, right) =>
      EBinary(walk(bop), walk(left), walk(right))
    case EVariadic(vop, exprs) =>
      EVariadic(walk(vop), walkList(exprs, walk))
    case EMathOp(mop, exprs) =>
      EMathOp(walk(mop), walkList(exprs, walk))
    case EConvert(cop, expr) =>
      EConvert(walk(cop), walk(expr))
    case ETypeOf(base) =>
      ETypeOf(walk(base))
    case EInstanceOf(expr, target) =>
      EInstanceOf(walk(expr), walk(target))
    case ETypeCheck(expr, ty) =>
      ETypeCheck(walk(expr), walk(ty))
    case EClo(fname, captured) =>
      EClo(walk(fname), walkList(captured, walk))
    case ECont(fname) =>
      ECont(walk(fname))
    case EDebug(expr) =>
      EDebug(walk(expr))
    case expr: ERandom     => walk(expr)
    case expr: AstExpr     => walk(expr)
    case expr: AllocExpr   => walk(expr)
    case expr: LiteralExpr => walk(expr)
  ).setLangOpt(expr.langOpt)

  // random number expressions
  def walk(rand: ERandom): ERandom = ERandom()

  // abstract syntax tree (AST) expressions
  def walk(ast: AstExpr): AstExpr = ast match
    case ESyntactic(name, args, rhsIdx, children) =>
      ESyntactic(
        walk(name),
        walkList(args, walk),
        walk(rhsIdx),
        walkList(children, walkOpt(_, walk)),
      )
    case ELexical(name, expr) =>
      ELexical(walk(name), walk(expr))

  // allocation expressions
  def walk(alloc: AllocExpr): AllocExpr = alloc match
    case ERecord(tnameOpt, fields) =>
      ERecord(
        walkOpt(tnameOpt, walk),
        walkList(fields, { case (p, e) => (walk(p), walk(e)) }),
      )
    case EMap(pairs) =>
      EMap(walkList(pairs, { case (k, v) => (walk(k), walk(v)) }))
    case EList(exprs) =>
      EList(walkList(exprs, walk))
    case EListConcat(exprs) =>
      EListConcat(walkList(exprs, walk))
    case ECopy(obj) =>
      ECopy(walk(obj))
    case EKeys(map, intSorted) =>
      EKeys(walk(map), walk(intSorted))
    case EGetChildren(ast) =>
      EGetChildren(walk(ast))
    case EGetItems(nt, ast) =>
      EGetItems(walk(nt), walk(ast))

  // literals
  def walk(lit: LiteralExpr): LiteralExpr = lit

  // unary operators
  def walk(uop: UOp): UOp = uop

  // binary operators
  def walk(bop: BOp): BOp = bop

  // variadic operators
  def walk(vop: VOp): VOp = vop

  // mathematical operators
  def walk(mop: MOp): MOp = mop

  // conversion operators
  def walk(cop: COp): COp = cop match
    case COp.ToStr(radix) => COp.ToStr(walkOpt(radix, walk))
    case op               => op

  // references
  def walk(ref: Ref): Ref = (ref match
    case Field(ref, expr) => Field(walk(ref), walk(expr))
    case x: Var           => walk(x)
  ).setLangOpt(ref.langOpt)

  // identifiers
  def walk(x: Var): Var = x match
    case Global(x)    => Global(walk(x))
    case local: Local => walk(local)
  def walk(x: Local): Local = x match
    case x: Name => walk(x)
    case x: Temp => walk(x)

  // named local identifiers
  def walk(x: Name): Name = Name(walk(x.name))
  def walk(x: Temp): Temp = Temp(walk(x.idx))

  // types
  def walk(ty: Type): Type = Type(ty.ty)
}
