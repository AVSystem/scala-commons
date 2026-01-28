package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.JavaCodecs.*

import scala.annotation.nowarn

trait SimpleIOCodecTest extends AbstractCodecTest {
  type Raw = Any

  def writeToOutput(write: Output => Unit): Any = {
    var result: Any = null
    write(new SimpleValueOutput(result = _))
    result
  }

  def createInput(raw: Any): Input = new SimpleValueInput(raw)
}

class SimpleGenCodecRoundtripTest extends GenCodecRoundtripTest with SimpleIOCodecTest

@nowarn
class SimpleGenCodecTest extends SimpleIOCodecTest {

  import CodecTestData.*

  test("java collections") {
    testWrite[JCollection[Int]](jArrayList, List(1, 2, 3))
    testWrite[JList[Int]](jArrayList, List(1, 2, 3))
    testWrite[JArrayList[Int]](jArrayList, List(1, 2, 3))
    testWrite[JLinkedList[Int]](jLinkedList, List(1, 2, 3))
    testWrite[JSet[Int]](jHashSet, List(1, 2, 3))
    testWrite[JHashSet[Int]](jHashSet, List(1, 2, 3))
    testWrite[JLinkedHashSet[Int]](jLinkedHashSet, List(1, 2, 3))
    testWrite[JSortedSet[Int]](jTreeSet, List(1, 2, 3))
    testWrite[JNavigableSet[Int]](jTreeSet, List(1, 2, 3))
    testWrite[JTreeSet[Int]](jTreeSet, List(1, 2, 3))
    testWrite[JMap[String, Int]](jHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWrite[JHashMap[String, Int]](jHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWrite[JLinkedHashMap[String, Int]](jLinkedHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWrite[JHashMap[Int, Int]](jIntHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
  }

  test("NoState") {
    type NoState = Nothing { type Dummy = Nothing }
    assert(summon[GenCodec[NoState]] == GenCodec.given_GenCodec_Nothing)
  }

  test("collections and wrappers") {
    testWrite[Option[Int]](some, 42)
    testWrite[Option[Int]](none, null)
    testWrite[Either[Int, String]](Left(42), Map("Left" -> 42))
    testWrite[Either[Int, String]](Right("lol"), Map("Right" -> "lol"))
    testWrite[BSeq[Int]](list, list)
    testWrite[ISeq[Int]](list, list)
    testWrite[List[Int]](list, list)
    testWrite[Set[Int]](set, set.toList)
    testWrite[Map[String, Int]](map, map)
    testWrite[Map[Int, Int]](intMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWrite[IHashMap[String, Int]](hashMap, hashMap)
  }

  test("tuples") {
    testWrite((1, 2, 3), List(1, 2, 3))
    testWrite((1, "lol"), List(1, "lol"))
    testWrite((1, "lol", 3.0, 'a', List("dafuq", "fuu")), List(1, "lol", 3.0, "a", List("dafuq", "fuu")))
  }

  test("object") {
    testWrite(
      SomeObject,
      Map(),
//      Map("random" -> 42)
    )
  }

  test("no arg case class") {
    testWrite(NoArgCaseClass(), Map())
  }

  test("single arg case class") {
    testWrite(SingleArgCaseClass("something"), Map("str" -> "something"))
  }

  test("transparent wrapper") {
    testWrite(TransparentWrapper("something"), "something")
  }

  test("transparent wrapper with dependency") {
    testWrite(TransparentWrapperWithDependency("something"), "something")
  }

//  test("transparent wrapper companion") {
//    testWrite(StringId("lolfuu"), "lolfuu")
//  }

  test("case class") {
    testWrite(
      SomeCaseClass("dafuq", List(1, 2, 3)),
      Map("some.str" -> "dafuq", "intList" -> List(1, 2, 3)),
    )
  }

  test("case class with wildcard") {
    testWrite(CaseClassWithWildcard(Stuff("lol")), Map("stuff" -> "lol"))
  }

  test("case class with optional fields") {
    testWrite(CaseClassWithOptionalFields("foo", Opt(42), Some(true)), Map("str" -> "foo", "int" -> 42, "bul" -> true))
    testWrite(CaseClassWithOptionalFields("foo", Opt.Empty, Some(true)), Map("str" -> "foo", "bul" -> true))
    testWrite(CaseClassWithOptionalFields("foo", Opt.Empty, None), Map("str" -> "foo"))
  }

  test("case class with auto optional fields") {
    testWrite(
      CaseClassWithAutoOptionalFields("foo", Opt(42), Some(true), NOpt(Opt(123))),
      Map("str" -> "foo", "int" -> 42, "bul" -> true, "nint" -> 123),
    )
    testWrite(
      CaseClassWithAutoOptionalFields("foo", Opt.Empty, Some(true), NOpt(Opt.Empty)),
      Map("str" -> "foo", "bul" -> true, "nint" -> null),
    )
    testWrite(CaseClassWithAutoOptionalFields("foo", Opt.Empty, None, NOpt.empty), Map("str" -> "foo"))
  }

//  test("reading nulls in case class with auto optional fields") {
//    testRead(
//      Map("str" -> "foo", "int" -> null, "bul" -> true, "nint" -> null),
//      CaseClassWithAutoOptionalFields("foo", Opt.Empty, Some(true), NOpt(Opt.Empty)),
//    )
//    testRead(
//      Map("str" -> "foo", "int" -> null, "bul" -> null),
//      CaseClassWithAutoOptionalFields("foo", Opt.Empty, None, NOpt.Empty),
//    )
//  }

  test("case class like") {
    testWrite(CaseClassLike("dafuq", List(1, 2, 3)), Map("some.str" -> "dafuq", "intList" -> List(1, 2, 3)))
  }

  test("case class like with inherited apply/unapply") {
    testWrite(HasInheritedApply("dafuq", List(1, 2, 3)), Map("a" -> "dafuq", "lb" -> List(1, 2, 3)))
  }

  test("apply/unapply provider based codec") {
    testWrite(ThirdParty(42, "lol"), Map("str" -> "lol", "int" -> 42))
  }

  test("varargs case class") {
    testWrite(VarargsCaseClass(42, "foo", "bar"), Map("int" -> 42, "strings" -> List("foo", "bar")))
  }

  test("only varargs case class") {
    testWrite(OnlyVarargsCaseClass("42", "420"), Map("strings" -> List("42", "420")))
  }

  test("varargs case class like") {
    testWrite(VarargsCaseClassLike("dafuq", 1, 2, 3), Map("some.str" -> "dafuq", "ints" -> List(1, 2, 3)))
  }

  test("only varargs case class like") {
    testWrite(OnlyVarargsCaseClassLike("dafuq", "indeed"), Map("strings" -> List("dafuq", "indeed")))
  }

  test("case class with default values") {
    testWrite(HasDefaults(str = "lol"), Map("str" -> "lol"))
    testWrite(HasDefaults(43, "lol"), Map("int" -> 43, "str" -> "lol"))
    testWrite(HasDefaults(str = null.asInstanceOf[String]), Map("str" -> null))
    testWrite(HasDefaults(str = "dafuq"), Map())
  }

  // test type dealiasing during materialization
  type IntTree = Tree[Int]

//  test("recursive generic case class") {
//    testWrite(
//      Node(
//        123,
//        List(
//          Node(
//            42,
//            List(
//              Node(52),
//              Node(53),
//            ),
//          ),
//          Node(43),
//        ),
//      ),
//      Map[String, Any](
//        "value" -> 123,
//        "children" -> List(
//          Map[String, Any](
//            "value" -> 42,
//            "children" -> List(
//              Map[String, Any]("value" -> 52, "children" -> List()),
//              Map[String, Any]("value" -> 53, "children" -> List()),
//            ),
//          ),
//          Map[String, Any]("value" -> 43, "children" -> List()),
//        ),
//      ),
//    )
//  }
//
//  test("recursively defined sealed hierarchy with explicit case class codec") {
//    testWrite[CustomList](CustomTail, Map("CustomTail" -> Map()))
//    testWrite[CustomList](
//      CustomCons(CustomCons(CustomTail)),
//      Map("CustomCons" -> Map("CustomCons" -> Map("CustomTail" -> Map()))),
//    )
//  }

  test("value class") {
    testWrite(ValueClass("costam"), Map("str" -> "costam"))
  }

//  test("sealed hierarchy") {
//    testWrite[SealedBase](SealedBase.CaseObject, Map("CaseObject" -> Map()))
//    testWrite[SealedBase](SealedBase.CaseClass("fuu"), Map("CaseClass" -> Map("str" -> "fuu")))
//    testWrite[SealedBase](SealedBase.InnerBase.InnerCaseObject, Map("InnerCaseObject" -> Map()))
//    testWrite[SealedBase](SealedBase.InnerBase.InnerCaseClass("fuu"), Map("InnerCaseClass" -> Map("str" -> "fuu")))
//  }

//  test("flat sealed hierarchy") {
//    testWrite[FlatSealedBase](
//      FlatSealedBase.FirstCase("fuu", 42),
//      Map("_case" -> "FirstCase", "_id" -> "fuu", "int" -> 42, "upper_id" -> "FUU"),
//    )
//    testWrite[FlatSealedBase](
//      FlatSealedBase.SecondCase("bar", 3.14, 1.0, 2.0),
//      Map("_case" -> "SecondCase", "_id" -> "bar", "dbl" -> 3.14, "moar" -> List(1.0, 2.0), "upper_id" -> "BAR"),
//    )
//    testWrite[FlatSealedBase](
//      FlatSealedBase.ThirdCase,
//      Map("_case" -> "ThirdCase", "_id" -> "third", "upper_id" -> "THIRD"),
//    )
//    testWrite[FlatSealedBase](
//      FlatSealedBase.RecursiveCase("rec", Opt(FlatSealedBase.ThirdCase)),
//      Map(
//        "_case" -> "RecursiveCase",
//        "_id" -> "rec",
//        "upper_id" -> "REC",
//        "sub" -> Map("_case" -> "ThirdCase", "_id" -> "third", "upper_id" -> "THIRD"),
//      ),
//    )
//  }

//  test("flat sealed hierarchy with transparent case") {
//    testWrite[TransparentFlatSealedBase](
//      TransparentCaseWrap(TransparentFlatThing(42, "foo")),
//      Map("_case" -> "TransparentCaseWrap", "num" -> 42, "text" -> "foo"),
//    )
//  }

//  test("random field access dependent flat sealed hierarchy reading") {
//    testRead[FlatSealedBase](
//      ListMap("_id" -> "fuu", "int" -> 42, "upper_id" -> "FUU", "_case" -> "FirstCase"),
//      FlatSealedBase.FirstCase("fuu", 42),
//    )
//  }

//  test("out of order field in flat sealed hierarchy") {
//    testRead[FlatSealedBase](
//      Map("_id" -> "fuu", "upper_id" -> "FUU", "random" -> 13, "_case" -> "FirstCase", "int" -> 42),
//      FlatSealedBase.FirstCase("fuu", 42),
//    )
//    testRead[FlatSealedBase](
//      Map(
//        "_id" -> "bar",
//        "upper_id" -> "FUU",
//        "random" -> 13,
//        "_case" -> "SecondCase",
//        "dbl" -> 3.14,
//        "moar" -> List(1.0, 2.0),
//      ),
//      FlatSealedBase.SecondCase("bar", 3.14, 1.0, 2.0),
//    )
//  }

//  test("GADT") {
//    testWrite[Expr[_]](NullExpr, Map("NullExpr" -> Map()))
//    testWrite[Expr[_]](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
//    testWrite[Expr[String]](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
//    testWrite[Expr[Int]](IntExpr(42), Map("IntExpr" -> Map("int" -> 42)))
//    testWrite[BaseExpr](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
//    testWrite[BaseExpr { type Value = String }](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
//  }

  test("recursive GADT") {
    testWrite[RecExpr[Int]](IntRecExpr(42), Map("_case" -> "IntRecExpr", "int" -> 42))
    testWrite[RecExpr[Int]](NothingRecExpr, Map("_case" -> "NothingRecExpr"))
    testWrite[RecExpr[Int]](ArbitraryRecExpr(42), Map("_case" -> "ArbitraryRecExpr", "value" -> 42))
    testWrite[RecExpr[Int]](
      LazyRecExpr(IntRecExpr(42)),
      Map("_case" -> "LazyRecExpr", "expr" -> Map("_case" -> "IntRecExpr", "int" -> 42)),
    )
    testWrite[RecExpr[RecBounded]](
      RecBoundedExpr(RecBounded(42)),
      Map("_case" -> "RecBoundedExpr", "value" -> Map("int" -> 42)),
    )
  }
//
//  test("pure GADT") {
//    testWrite[PureGadtExpr[String]](StringLiteral("str"), Map("_case" -> "StringLiteral", "value" -> "str"))
//    testWrite[PureGadtExpr[String]](
//      Plus(StringLiteral("str"), StringLiteral("fag")),
//      Map(
//        "_case" -> "Plus",
//        "lhs" -> Map("_case" -> "StringLiteral", "value" -> "str"),
//        "rhs" -> Map("_case" -> "StringLiteral", "value" -> "fag"),
//      ),
//    )
//  }
  type IntBranch = Branch[Int]
//  GenCodec.derived[IntTree]
  //  GenCodec.derived[IntBranch]

  case class Node[T](value: T, children: List[Node[T]] = Nil) derives GenCodec

//  test("recursive generic ADT") {
//    testWrite[Tree[Int]](
//      Branch(
//        Leaf(1),
//        Branch(
//          Leaf(2),
//          Leaf(3),
//        ),
//      ),
//      Map(
//        "Branch" -> Map(
//          "left" -> Map("Leaf" -> Map("value" -> 1)),
//          "right" -> Map(
//            "Branch" -> Map(
//              "left" -> Map("Leaf" -> Map("value" -> 2)),
//              "right" -> Map("Leaf" -> Map("value" -> 3)),
//            ),
//          ),
//        ),
//      ),
//    )
//  }

  test("sealed enum") {
    testWrite[Enumz](Enumz.First, Map("Primary" -> Map()))
    testWrite[Enumz](Enumz.Second, Map("Second" -> Map()))
    testWrite[Enumz](Enumz.Third, Map("Third" -> Map()))
  }

  test("sealed enum based on key codec") {
    testWrite[KeyEnumz](KeyEnumz.First, "Primary")
    testWrite[KeyEnumz](KeyEnumz.Second, "Second")
    testWrite[KeyEnumz](KeyEnumz.Third, "Third")
  }

//  test("typed map") {
//    import SealedKey.*
//
//    testWrite(
//      TypedMap(StringKey -> "lol", IntKey -> 42, BooleanKey -> true),
//      Map[String, Any]("StringKey" -> "lol", "IntKey" -> 42, "BooleanKey" -> true),
//    )
//  }

//  test("customized flat sealed hierarchy") {
//    testWrite[CustomizedSeal](CustomizedCase("dafuq"), Map("str" -> "dafuq"))
//    testWrite[CustomizedSeal](CustomizedObjekt, Map("kejs" -> "CustomizedObjekt"))
//  }

  test("case class with more than 22 fields") {
    val inst = ItsOverTwentyTwo(
      "v1",
      "v2",
      "v3",
      "v4",
      "v5",
      "v6",
      "v7",
      "v8",
      "v9",
      "v10",
      "v11",
      "v12",
      "v13",
      "v14",
      "v15",
      "v16",
      "v17",
      "v18",
      "v19",
      "v20",
      "v21",
      "v22",
      "v23",
    )
    val repr = (1 to 23).map(i => s"a$i" -> s"v$i").toMap
    testWrite[ItsOverTwentyTwo](inst, repr)
  }

//  test("recursive materialization with intermediate sequence") {
//    testWrite[HasColl](
//      HasCollCase(List(DepCase("kek"))),
//      Map("_case" -> "HasCollCase", "coll" -> List(Map("_case" -> "DepCase", "str" -> "kek"))),
//    )
//  }

  test("refined sealed type with type member") {
    testWrite[SealedRefined { type X = Int }](
      SealedRefined.First(42),
      Map("First" -> Map("foo" -> 42)),
    )
  }

  test("recursive materialization of indirectly recursive type") {
    def testWithCodec(implicit codec: GenCodec[StepOne]): Unit = {
      testWrite[StepOne](StepOne(StepTwo(Opt.Empty)), Map("stepTwo" -> Map("stepOne" -> null)))
      testWrite[StepOne](
        StepOne(StepTwo(Opt(StepOne(StepTwo(Opt.Empty))))),
        Map("stepTwo" -> Map("stepOne" -> Map("stepTwo" -> Map("stepOne" -> null)))),
      )
    }
    testWithCodec(GenCodec.materializeRecursively)
    testWithCodec {
      implicit val implCodec: GenCodec[StepOne] = GenCodec.materializeRecursively
      implCodec
    }
  }

//  test("auto materialized key codec") {
//    testWrite[Map[ThingId, ThingId]](Map(ThingId("a") -> ThingId("b")), Map("a" -> "b"))
//  }

  test("Java builder based codec") {
    testWrite[BuildablePojo](BuildablePojo.builder().build(), Map())
    testWrite[BuildablePojo](BuildablePojo.builder().setStr("foo").build(), Map("str" -> "foo"))
    testWrite[BuildablePojo](
      BuildablePojo.builder().setStr("foo").setFlags(JList(true, false)).setCool(false).build(),
      Map("str" -> "foo", "flags" -> List(true, false), "cool" -> false),
    )
  }

  test("generated fields") {
    testWrite[Generator](
      Generator("v"),
      Map(
        "value" -> "v",
        "upper" -> "V",
        "abstractUpper" -> "V",
        "valUpper" -> "V",
        "getterUpper" -> "V",
        "varUpper" -> "V",
        "lazyValUpper" -> "V",
      ),
    )
  }
}
