package esmeta.ir.util

import esmeta.util.BasicWalker
import esmeta.ir.*

/** a walker for IR */
trait Walker extends BasicWalker {
  def walk(elem: IRElem): IRElem = elem match
    case elem: Program    => walk(elem)
    case elem: Func       => walk(elem)
    case elem: Func.Kind  => walk(elem)
    case elem: Func.Param => walk(elem)
    case elem: Inst       => walk(elem)
    case elem: Expr       => walk(elem)
    case elem: UOp        => walk(elem)
    case elem: BOp        => walk(elem)
    case elem: COp        => walk(elem)
    case elem: Ref        => walk(elem)
    case elem: Type       => walk(elem)

  // programs
  def walk(program: Program): Program =
    val newProgram = Program(walkList(program.funcs, walk))
    newProgram.spec = program.spec
    newProgram

  // functions
  def walk(func: Func): Func =
    val Func(main, kind, name, ps, body, algo) = func
    Func(
      walk(main),
      walk(kind),
      walk(name),
      walkList(ps, walk),
      walk(func.body),
      algo,
    )

  // function kinds
  def walk(kind: Func.Kind): Func.Kind = kind

  // function parameters
  def walk(param: Func.Param): Func.Param =
    val Func.Param(name, opt, ty) = param
    Func.Param(walk(name), walk(opt), walk(ty))

  // instructions
  def walk(inst: Inst): Inst = inst match
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
    case ILoop(k, c, b)         => ILoop(walk(k), walk(c), walk(b))
    case ICall(l, f, as)        => ICall(walk(l), walk(f), walkList(as, walk))

  // expressions
  def walk(expr: Expr): Expr = expr match
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
    case EGrammar(name, params) =>
      EGrammar(walk(name), walkList(params, walk))
    case ESourceText(exor) =>
      ESourceText(walk(expr))
    case EYet(msg) =>
      EYet(walk(msg))
    case EContains(list, elem) =>
      EContains(walk(list), walk(elem))
    case EStrConcat(exprs) =>
      EStrConcat(walkList(exprs, walk))
    case ESubstring(expr, from, to) =>
      ESubstring(walk(expr), walk(from), walk(to))
    case ERef(ref) =>
      ERef(walk(ref))
    case EUnary(uop, expr) =>
      EUnary(walk(uop), walk(expr))
    case EBinary(bop, left, right) =>
      EBinary(walk(bop), walk(left), walk(right))
    case EVariadic(vop, exprs) =>
      EVariadic(walk(vop), walkList(exprs, walk))
    case EConvert(cop, expr) =>
      EConvert(walk(cop), walk(expr))
    case ETypeOf(base) =>
      ETypeOf(walk(base))
    case ETypeCheck(expr, ty) =>
      ETypeCheck(walk(expr), walk(ty))
    case EClo(fname, captured) =>
      EClo(walk(fname), walkList(captured, walk))
    case ECont(fname) =>
      ECont(walk(fname))
    case expr: AstExpr     => walk(expr)
    case expr: AllocExpr   => walk(expr)
    case expr: LiteralExpr => walk(expr)

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
    case EMap(tname, fields, asite) =>
      EMap(
        walk(tname),
        walkList(fields, { case (p, e) => (walk(p), walk(e)) }),
        walk(asite),
      )
    case EList(exprs, asite) =>
      EList(walkList(exprs, walk), walk(asite))
    case EListConcat(exprs, asite) =>
      EListConcat(walkList(exprs, walk), walk(asite))
    case ESymbol(desc, asite) =>
      ESymbol(walk(desc), walk(asite))
    case ECopy(obj, asite) =>
      ECopy(walk(obj), walk(asite))
    case EKeys(map, intSorted, asite) =>
      EKeys(walk(map), walk(intSorted), walk(asite))

  // literals
  def walk(lit: LiteralExpr): LiteralExpr = lit

  // unary operators
  def walk(uop: UOp): UOp = uop

  // binary operators
  def walk(bop: BOp): BOp = bop

  // variadic operators
  def walk(vop: VOp): VOp = vop

  // conversion operators
  def walk(cop: COp): COp = cop match
    case COp.ToStr(radix) => COp.ToStr(walkOpt(radix, walk))
    case op               => op

  // references
  def walk(ref: Ref): Ref = ref match
    case Prop(ref, expr) => Prop(walk(ref), walk(expr))
    case x: Id           => walk(x)

  // identifiers
  def walk(x: Id): Id = x match
    case Global(x) => Global(walk(x))
    case x: Name   => walk(x)
    case Temp(k)   => Temp(walk(k))

  // named local identifiers
  def walk(x: Name): Name = Name(walk(x.name))

  // TODO types
  def walk(ty: Type): Type = Type(walk(ty.name))
}