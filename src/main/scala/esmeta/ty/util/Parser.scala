package esmeta.ty.util

import esmeta.state.{Nt, Number, Math}
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** metalanguage parser */
object Parser extends Parsers
trait Parsers extends BasicParsers {

  override protected val whiteSpace = whiteSpaceWithComment

  // type model
  given tyModel: Parser[TyModel] = {
    rep(tyDecl) ^^ { case ds => TyModel(ds.map(x => x.name -> x).toMap) }
  }.named("ty.TyModel")

  // type declarations
  given tyDecl: Parser[TyDecl] = {
    val extend = "extends " ~> ident
    "type " ~> ident ~ opt(extend) ~ opt("{" ~> rep(field) <~ "}") ^^ {
      case x ~ p ~ ms => TyDecl(x, p, ms.getOrElse(Nil).toMap)
    }
  }.named("ty.TyDecl")

  // types
  given ty: Parser[Ty] = {
    unknownTy |
    valueTy
  }.named("ty.Ty")

  lazy val unknownTy: Parser[UnknownTy] = {
    "Unknown" ~> opt("[" ~> string <~ "]") ^^ { UnknownTy(_) }
  }.named("ty.UnknownTy")

  lazy val valueTy: Parser[ValueTy] = {
    rep1sep(singleValueTy, "|") ^^ {
      case ts => ts.foldLeft(ValueTy.Bot)(_ || _)
    }
  }.named("ty.ValueTy")

  private lazy val singleValueTy: Parser[ValueTy] = {
    "Any" ^^^ AnyT ||| (
      singleCompTy ^^ { case t => ValueTy(comp = t) } |
      singleMapTy ^^ { case t => ValueTy(map = t) } |
      singlePureValueTy ^^ { case t => ValueTy(pureValue = t) }
    )
  }.named("ty.ValueTy (single)")

  /** completion record types */
  given compTy: Parser[CompTy] = {
    rep1sep(singleCompTy, "|") ^^ { case ts => ts.foldLeft(CompTy.Bot)(_ || _) }
  }.named("ty.CompTy")

  private lazy val singleCompTy: Parser[CompTy] = {
    "Normal" ~> opt("[" ~> pureValueTy <~ "]") ^^ {
      case v => CompTy(normal = v.getOrElse(PureValueTy.Top))
    } | "Abrupt" ~> opt("[" ~> rep1(ident) <~ "]") ^^ {
      case s => CompTy(abrupt = s.fold(Inf)(Fin(_: _*)))
    }
  }.named("ty.CompTy (single)")

  /** pure value types (non-completion record types) */
  given pureValueTy: Parser[PureValueTy] = {
    rep1sep(singlePureValueTy, "|") ^^ {
      case ts => ts.foldLeft(PureValueTy.Bot)(_ || _)
    }
  }.named("ty.PureValueTy")

  private lazy val singlePureValueTy: Parser[PureValueTy] = {
    // any pure value
    "PureValue" ^^^ PureValueTy.Top |
    // ECMAScript value
    "ESValue" ^^^ ESValueT.pureValue |
    // closure
    "Clo[" ~> rep1sep(string, ",") <~ "]" ^^ {
      case s => PureValueTy(clo = Fin(s.toSet))
    } | "Clo" ^^^ PureValueTy(clo = Inf) |
    // continuation
    "Cont[" ~> rep1sep(int, ",") <~ "]" ^^ {
      case s => PureValueTy(cont = Fin(s.toSet))
    } | "Cont" ^^^ PureValueTy(cont = Inf) |
    // new record
    newRecordTy ^^ { case r => ??? } |
    // record
    singleRecordTy ^^ { case r => PureValueTy(record = r) } |
    // list
    singleListTy ^^ { case l => PureValueTy(list = l) } |
    // AST value
    singleAstValueTy ^^ { case ast => PureValueTy(astValue = ast) } |
    // nt
    "Nt[" ~> rep1sep(nt, ",") <~ "]" ^^ {
      case s => PureValueTy(nt = Fin(s.toSet))
    } | "Nt" ^^^ PureValueTy(nt = Inf) |
    // code unit
    "CodeUnit" ^^^ PureValueTy(codeUnit = true) |
    // enum
    "Enum[" ~> rep1sep(enumv, ",") <~ "]" ^^ {
      case s => PureValueTy(enumv = Fin(s.toSet))
    } |
    // mathematical value
    singleMathTy ^^ { case m => PureValueTy(math = m) } |
    // infinity
    singleInfinityTy ^^ { case i => PureValueTy(infinity = i) } |
    // number
    singleNumberTy ^^ { case n => PureValueTy(number = n) } |
    // big integer
    "BigInt" ^^^ PureValueTy(bigInt = true) |
    // string
    "String[" ~> rep1sep(string, ",") <~ "]" ^^ {
      case s => PureValueTy(str = Fin(s.toSet))
    } | "String" ^^^ PureValueTy(str = Inf) |
    // boolean
    singleBoolTy ^^ { case b => PureValueTy(bool = b) } |
    // undefined
    "Undefined" ^^^ PureValueTy(undef = true) |
    // null
    "Null" ^^^ PureValueTy(nullv = true) |
    // absent
    "Absent" ^^^ PureValueTy(absent = true) |
    // name
    singleNameTy ^^ { case name => PureValueTy(name = name) }
  }.named("ty.PureValueTy (single)")

  private lazy val numberWithSpecial: Parser[Number] =
    doubleWithSpecial ^^ { Number(_) }
  private lazy val doubleWithSpecial: Parser[Double] =
    double |
    ("+INF" | "INF") ^^^ Double.PositiveInfinity |
    "-INF" ^^^ Double.NegativeInfinity |
    "NaN" ^^^ Double.NaN

  private lazy val nt: Parser[Nt] =
    ("|" ~> word <~ "|") ~ opt(parseParams) ^^ {
      case x ~ ps => Nt(x, ps.getOrElse(Nil))
    }
  private lazy val parseParams: Parser[List[Boolean]] =
    opt("[" ~> rep(simpleBool) <~ "]") ^^ { _.getOrElse(Nil) }
  private lazy val simpleBool: Parser[Boolean] =
    "T" ^^^ true | "F" ^^^ false
  private lazy val enumv: Parser[String] =
    "~" ~> "[^~]+".r <~ "~"

  /** new record types */
  given newRecordTy: Parser[NewRecordTy] = {
    rep1sep(singleNewRecordTy, "|") ^^ {
      case ts => ts.foldLeft(NewRecordTy.Bot)(_ || _)
    }
  }.named("ty.NewRecordTy")

  private lazy val singleNewRecordTy: Parser[NewRecordTy] = {
    "Record" ^^^ NewRecordTy.Top |
    "Record[" ~> word ~ ("{" ~> rep1sep(field, ",") <~ "}") <~ "]" ^^ {
      case x ~ ps => NewRecordTy.Detail(x, ps.toMap)
    } |
    "Record[" ~> rep1sep(word, "|") <~ "]" ^^ {
      case xs => NewRecordTy.Simple(xs.toSet)
    }
  }.named("ty.NewRecordTy (single)")

  /** named record types */
  given nameTy: Parser[NameTy] = {
    rep1sep(singleNameTy, "|") ^^ {
      case ts => ts.foldLeft(NameTy.Bot)(_ || _)
    }
  }.named("ty.NameTy")

  private lazy val singleNameTy: Parser[NameTy] = {
    "AnyName" ^^^ NameTy.Top |
    not("Any") ~> camel ^^ { case name => NameTy(Fin(name)) }
  }.named("ty.NameTy (single)")

  /** record types */
  given recordTy: Parser[RecordTy] = {
    rep1sep(singleRecordTy, "|") ^^ {
      case ts => ts.foldLeft[RecordTy](RecordTy.Bot)(_ || _)
    }
  }.named("ty.RecordTy")

  private lazy val singleRecordTy: Parser[RecordTy] = {
    "AnyRecord" ^^^ RecordTy.Top |
    "{" ~> rep1sep(field, ",") <~ "}" ^^ {
      case pairs => RecordTy.Elem(pairs.toMap)
    }
  }.named("ty.RecordTy (single)")

  /** mathematical value types */
  given mathTy: Parser[MathTy] = {
    rep1sep(singleMathTy, "|") ^^ { case ts => ts.foldLeft(MathTy.Bot)(_ || _) }
  }.named("ty.MathTy")

  private lazy val singleMathTy: Parser[MathTy] =
    "Math[" ~> rep1sep(decimal, ",") <~ "]"
    ^^ { case ds => MathSetTy(ds.toSet.map(Math(_))) } |
    camel
    ^? {
      case "Int"       => IntTy
      case "NonPosInt" => NonPosIntTy
      case "NonNegInt" => NonNegIntTy
      case "NegInt"    => NegIntTy
      case "PosInt"    => PosIntTy
      case "Math"      => MathTopTy
    }

  /** infinity types */
  given infTy: Parser[InfinityTy] = {
    rep1sep(singleInfinityTy, "|") ^^ {
      case ts => ts.foldLeft(InfinityTy.Bot)(_ || _)
    }
  }.named("ty.InfinityTy")

  /** number types */
  given numberTy: Parser[NumberTy] = {
    rep1sep(singleNumberTy, "|") ^^ {
      case ts => ts.foldLeft(NumberTy.Bot)(_ || _)
    }
  }.named("ty.NumberTy")

  private lazy val singleNumberTy: Parser[NumberTy] =
    "NumberInt" ^^^ NumberIntTy |
    "Number[" ~> rep1sep(numberWithSpecial, ",") <~ "]" ^^ {
      case n => NumberSetTy(n.toSet)
    } | "Number" ^^^ NumberTopTy

  private lazy val singleInfinityTy: Parser[InfinityTy] =
    "INF" ^^^ InfinityTy.Top | "+INF" ^^^ InfinityTy.Pos | "-INF" ^^^ InfinityTy.Neg

  given boolTy: Parser[BoolTy] = {
    rep1sep(singleBoolTy, "|") ^^ { case ts => ts.foldLeft(BoolTy.Bot)(_ || _) }
  }.named("ty.BoolTy")

  private lazy val singleBoolTy: Parser[BoolTy] =
    // boolean
    "Boolean" ^^^ BoolTy(Set(false, true)) |
    "True" ^^^ BoolTy(Set(true)) |
    "False" ^^^ BoolTy(Set(false))

  private lazy val field: Parser[(String, ValueTy)] =
    word ~ opt(":" ~> valueTy) ^^ { case k ~ v => (k, v.getOrElse(AnyT)) }

  /** list types */
  given listTy: Parser[ListTy] = {
    rep1sep(singleListTy, "|") ^^ { case ts => ts.foldLeft(ListTy.Bot)(_ || _) }
  }.named("ty.ListTy")

  private lazy val singleListTy: Parser[ListTy] = {
    "List[" ~> valueTy <~ "]" ^^ { case v => ListTy(Some(v)) } |
    "List" ^^^ { ListTy.Top } |
    "Nil" ^^^ ListTy.Nil
  }.named("ty.ListTy (single)")

  /** AST value types */
  given astValueTy: Parser[AstValueTy] = {
    rep1sep(singleAstValueTy, "|") ^^ {
      case ts => ts.foldLeft[AstValueTy](AstValueTy.Bot)(_ || _)
    }
  }.named("ty.AstValueTy")

  private lazy val singleAstValueTy: Parser[AstValueTy] = {
    "Ast:" ~> word ~ ("[" ~> int) ~ ("," ~> int <~ "]") ^^ {
      case x ~ i ~ j => AstSingleTy(x, i, j)
    } | "Ast[" ~> repsep(word, ",") <~ "]" ^^ {
      case xs => AstNameTy(xs.toSet)
    } | "Ast" ^^^ AstTopTy
  }.named("ty.ListTy (single)")

  /** map types */
  given mapTy: Parser[MapTy] = {
    rep1sep(singleMapTy, "|") ^^ {
      case ts => ts.foldLeft(MapTy.Bot)(_ || _)
    }
  }.named("ty.MapTy")

  private lazy val singleMapTy: Parser[MapTy] = {
    "Map[" ~> pureValueTy ~
    ("|->" ~> pureValueTy) <~ "]" ^^ { case k ~ v => MapTy(k, v) }
  }.named("ty.MapTy (single)")
}
