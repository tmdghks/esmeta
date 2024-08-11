package esmeta.ty

import esmeta.ty.util.JsonProtocol.given
import esmeta.state.{GrammarSymbol, Number}
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*

/** JSON test */
class JsonTinyTest extends TyTest {
  val name: String = "tyJsonTest"

  // registration
  def init: Unit = {

    checkJsonWithString("TyModel")(
      tyModel0 -> "",
      tyModel1 -> """type A""",
      tyModel2 -> """type A extends B
      |
      |type A {
      |  abstract def a;
      |}""".stripMargin,
      tyModel3 -> """type A
      |
      |type A extends B {
      |  abstract def a;
      |}
      |
      |type A {
      |  abstract def a;
      |  def c?;
      |}""".stripMargin,
    )

    checkJsonWithString("TyDecl")(
      decl0 -> """type A""",
      decl1 -> """type A {
      |  abstract def a;
      |}""".stripMargin,
      decl2 -> """type A {
      |  abstract def a;
      |  def c?;
      |}""".stripMargin,
      declParent0 -> """type A extends B""",
      declParent1 -> """type A extends B {
      |  abstract def a;
      |}""".stripMargin,
      declParent2 -> """type A extends B {
      |  abstract def a;
      |  def c?;
      |}""".stripMargin,
    )

    checkJsonWithString("TyDecl.Elem")(
      absMethod -> "abstract def a",
      conMethod -> "def b",
      conMethodOpt -> "def c?",
      conMethodTarget -> "def d = foo",
      conMethodOptTarget -> "def e? = bar",
    )

    checkJsonWithString("FieldMap")(
      fieldMap0 -> "{}",
      fieldMap1 -> "{ p }".stripMargin,
      fieldMap2 -> "{ p, q : [U] Boolean, * : Number }",
      fieldMap3 -> "{ p, q : [A] Boolean, r : [UA] Null, * : [A] Bot }",
    )

    checkJsonWithString("Ty")(
      AnyT -> "Any",
      CompT -> "Completion",
      AbruptT -> "Abrupt",
      NormalT(NumberT) -> "Normal[Number]",
      MapT -> "Map",
      MapT(StrT, RecordT("Binding")) -> "Map[String -> Record[Binding]]",
      CloT -> "Clo",
      CloT("ToString:clo0") -> "Clo[\"ToString:clo0\"]",
      ContT -> "Cont",
      ContT(42, 3) -> "Cont[3, 42]",
      ESValueT -> "ESValue",
      UnknownTy() -> "Unknown",
      UnknownTy(Some("T")) -> "Unknown[\"T\"]",
      RecordT -> "Record",
      RecordT("Cat") -> "Record[Cat]",
      RecordT("Cat", "Dog") -> "Record[Cat | Dog]",
      RecordT("Object", Map("A" -> NumberT, "B" -> BoolT)) ->
      "Record[Object { A : Number, B : Boolean }]",
      RecordT(
        "",
        Map(
          "P" -> AnyT,
          "S" -> AnyT,
          "Q" -> NumberT,
          "R" -> BoolT,
        ),
      ) -> "Record[{ P, Q : Number, R : Boolean, S }]",
      NilT -> "Nil",
      ListT(NumberT) -> "List[Number]",
      SymbolT -> "Record[Symbol]",
      AstT -> "Ast",
      AstT("Literal") -> "Ast[Literal]",
      AstT("Member", 1) -> "Ast[Member[1]]",
      GrammarSymbolT(
        GrammarSymbol("Literal", List(true)),
        GrammarSymbol("Identifier", List(false, true, false)),
      ) -> "GrammarSymbol[|Identifier|[FTF], |Literal|[T]]",
      CodeUnitT -> "CodeUnit",
      EnumT("key") -> "Enum[~key~]",
      EnumT("key", "value") -> "Enum[~key~, ~value~]",
      MathT -> "Math",
      IntT -> "Int",
      NonPosIntT -> "NonPosInt",
      NonNegIntT -> "NonNegInt",
      NegIntT -> "NegInt",
      PosIntT -> "PosInt",
      MathT(0, 1) -> "Math[0, 1]",
      InfinityT -> "INF",
      NegInfinityT -> "-INF",
      PosInfinityT -> "+INF",
      NumberT -> "Number",
      NumberT(Number(Double.PositiveInfinity)) -> "Number[+INF]",
      NumberT(Number(Double.NegativeInfinity)) -> "Number[-INF]",
      NumberT(Number(Double.NaN)) -> "Number[NaN]",
      NumberT(
        Number(Double.PositiveInfinity),
        Number(Double.NegativeInfinity),
        Number(Double.NaN),
        Number(-0.0),
        Number(0.0),
      ) -> "Number[-INF, -0.0, 0.0, +INF, NaN]",
      BigIntT -> "BigInt",
      StrT -> "String",
      StrT("a") -> "String[\"a\"]",
      BoolT -> "Boolean",
      UndefT -> "Undefined",
      NullT -> "Null",
    )
  }

  init
}
