package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.TypedMap
import com.avsystem.commons.serialization.CodecTestData.{TransparentFlatSealedBase, _}
import com.avsystem.commons.serialization.JavaCodecs._

abstract class GenCodecRoundtripTest extends AbstractCodecTest {
  test("java collections") {
    testRoundtrip[JCollection[Int]](jArrayList)
    testRoundtrip[JList[Int]](jArrayList)
    testRoundtrip[JArrayList[Int]](jArrayList)
    testRoundtrip[JLinkedList[Int]](jLinkedList)
    testRoundtrip[JSet[Int]](jHashSet)
    testRoundtrip[JHashSet[Int]](jHashSet)
    testRoundtrip[JLinkedHashSet[Int]](jLinkedHashSet)
    testRoundtrip[JSortedSet[Int]](jTreeSet)
    testRoundtrip[JNavigableSet[Int]](jTreeSet)
    testRoundtrip[JTreeSet[Int]](jTreeSet)
    testRoundtrip[JMap[String, Int]](jHashMap)
    testRoundtrip[JHashMap[String, Int]](jHashMap)
    testRoundtrip[JLinkedHashMap[String, Int]](jLinkedHashMap)
    testRoundtrip[JHashMap[Int, Int]](jIntHashMap)
  }

  test("NoState") {
    type NoState = Nothing {type Dummy = Nothing}
    assert(implicitly[GenCodec[NoState]] == GenCodec.NothingCodec)
  }

  test("collections and wrappers") {
    testRoundtrip[Option[Int]](some)
    testRoundtrip[Option[Int]](none)
    testRoundtrip[Either[Int, String]](Left(42))
    testRoundtrip[Either[Int, String]](Right("lol"))
    testRoundtrip[BSeq[Int]](list)
    testRoundtrip[ISeq[Int]](list)
    testRoundtrip[List[Int]](list)
    testRoundtrip[Set[Int]](set)
    testRoundtrip[Map[String, Int]](map)
    testRoundtrip[Map[Int, Int]](intMap)
    testRoundtrip[IHashMap[String, Int]](hashMap)
  }

  test("tuples") {
    testRoundtrip((1, 2, 3))
    testRoundtrip((1, "lol"))
    testRoundtrip((1, "lol", 3.0, 'a', List("dafuq", "fuu")))
  }

  test("object") {
    testRoundtrip(SomeObject)
  }

  test("no arg case class") {
    testRoundtrip(NoArgCaseClass())
  }

  test("single arg case class") {
    testRoundtrip(SingleArgCaseClass("something"))
  }

  test("transparent wrapper") {
    testRoundtrip(TransparentWrapper("something"))
  }

  test("transparent wrapper with dependency") {
    testRoundtrip(TransparentWrapperWithDependency("something"))
  }

  test("transparent wrapper companion") {
    testRoundtrip(StringId("lolfuu"), "lolfuu")
  }

  test("case class") {
    testRoundtrip(SomeCaseClass("dafuq", List(1, 2, 3)))
  }

  test("case class with wildcard") {
    testRoundtrip(CaseClassWithWildcard(Stuff("lol")))
  }

  test("case class with optional fields") {
    testRoundtrip(CaseClassWithOptionalFields("foo", Opt(42), Some(true)))
    testRoundtrip(CaseClassWithOptionalFields("foo", Opt.Empty, Some(true)))
    testRoundtrip(CaseClassWithOptionalFields("foo", Opt.Empty, None))
  }

  test("case class with auto optional fields") {
    testRoundtrip(CaseClassWithAutoOptionalFields("foo", Opt(42), Some(true), NOpt(Opt(123))))
    testRoundtrip(CaseClassWithAutoOptionalFields("foo", Opt.Empty, Some(true), NOpt(Opt.Empty)))
    testRoundtrip(CaseClassWithAutoOptionalFields("foo", Opt.Empty, None, NOpt.empty))
  }

  test("case class like") {
    testRoundtrip(CaseClassLike("dafuq", List(1, 2, 3)))
  }

  test("case class like with inherited apply/unapply") {
    testRoundtrip(HasInheritedApply("dafuq", List(1, 2, 3)))
  }

  test("apply/unapply provider based codec") {
    testRoundtrip(ThirdParty(42, "lol"))
  }

  test("varargs case class") {
    testRoundtrip(VarargsCaseClass(42, "foo", "bar"))
  }

  test("only varargs case class") {
    testRoundtrip(OnlyVarargsCaseClass("42", "420"))
  }

  test("varargs case class like") {
    testRoundtrip(VarargsCaseClassLike("dafuq", 1, 2, 3))
  }

  test("only varargs case class like") {
    testRoundtrip(OnlyVarargsCaseClassLike("dafuq", "indeed"))
  }

  test("case class with default values") {
    testRoundtrip(HasDefaults(str = "lol"))
    testRoundtrip(HasDefaults(43, "lol"))
    testRoundtrip(HasDefaults(str = null))
    testRoundtrip(HasDefaults(str = "dafuq"))
  }

  case class Node[T](value: T, children: List[Node[T]] = Nil)
  object Node extends HasPolyGenCodec[Node]

  test("recursive generic case class") {
    testRoundtrip(Node(123, List(Node(42, List(Node(52), Node(53))), Node(43))))
  }

  test("recursively defined sealed hierarchy with explicit case class codec") {
    testRoundtrip[CustomList](CustomTail)
    testRoundtrip[CustomList](CustomCons(CustomCons(CustomTail)))
  }

  test("value class") {
    testRoundtrip(ValueClass("costam"), Map("str" -> "costam"))
  }

  test("sealed hierarchy") {
    testRoundtrip[SealedBase](SealedBase.CaseObject)
    testRoundtrip[SealedBase](SealedBase.CaseClass("fuu"))
    testRoundtrip[SealedBase](SealedBase.InnerBase.InnerCaseObject)
    testRoundtrip[SealedBase](SealedBase.InnerBase.InnerCaseClass("fuu"))
  }

  test("flat sealed hierarchy") {
    testRoundtrip[FlatSealedBase](FlatSealedBase.FirstCase("fuu"))
    testRoundtrip[FlatSealedBase](FlatSealedBase.SecondCase("bar", 3.14, 1.0, 2.0))
    testRoundtrip[FlatSealedBase](FlatSealedBase.ThirdCase)
    testRoundtrip[FlatSealedBase](FlatSealedBase.RecursiveCase("rec", Opt(FlatSealedBase.ThirdCase)))
  }

  test("flat sealed hierarchy with transparent cases") {
    testRoundtrip[TransparentFlatSealedBase](TransparentCaseWrap(TransparentFlatThing(42, "fuu")))
  }

  test("GADT") {
    testRoundtrip[Expr[_]](NullExpr)
    testRoundtrip[Expr[_]](StringExpr("stringzor"))
    testRoundtrip[Expr[String]](StringExpr("stringzor"))
    testRoundtrip[Expr[Int]](IntExpr(42))
    testRoundtrip[BaseExpr](StringExpr("stringzor"))
    testRoundtrip[BaseExpr {type Value = String}](StringExpr("stringzor"))
  }

  test("recursive GADT") {
    testRoundtrip[RecExpr[Int]](IntRecExpr(42))
    testRoundtrip[RecExpr[Int]](NothingRecExpr)
    testRoundtrip[RecExpr[Int]](ArbitraryRecExpr(42))
    testRoundtrip[RecExpr[Int]](LazyRecExpr(IntRecExpr(42)))
    testRoundtrip[RecExpr[RecBounded]](RecBoundedExpr(RecBounded(42)))
  }

  test("pure GADT") {
    testRoundtrip[PureGadtExpr[String]](StringLiteral("str"))
    testRoundtrip[PureGadtExpr[String]](Plus(StringLiteral("str"), StringLiteral("fag")))
  }

  // test type dealiasing during materialization
  type IntTree = Tree[Int]
  GenCodec.materialize[IntTree]
  type IntBranch = Branch[Int]
  GenCodec.materialize[IntBranch]

  test("recursive generic ADT") {
    testRoundtrip[Tree[Int]](Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
  }

  test("sealed enum") {
    testRoundtrip[Enumz](Enumz.First)
    testRoundtrip[Enumz](Enumz.Second)
    testRoundtrip[Enumz](Enumz.Third)
  }

  test("sealed enum based on key codec") {
    testRoundtrip[KeyEnumz](KeyEnumz.First)
    testRoundtrip[KeyEnumz](KeyEnumz.Second)
    testRoundtrip[KeyEnumz](KeyEnumz.Third)
  }

  test("typed map") {
    import SealedKey._
    testRoundtrip(TypedMap(StringKey -> "lol", IntKey -> 42, BooleanKey -> true))
  }

  test("customized flat sealed hierarchy") {
    testRoundtrip[CustomizedSeal](CustomizedCase("dafuq"))
    testRoundtrip[CustomizedSeal](CustomizedObjekt)
  }

  test("case class with more than 22 fields") {
    val inst = ItsOverTwentyTwo(
      "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10",
      "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20",
      "v21", "v22", "v23")
    testRoundtrip[ItsOverTwentyTwo](inst)
  }

  test("recursive materialization with intermediate sequence") {
    testRoundtrip[HasColl](HasCollCase(List(DepCase("kek"))))
  }

  test("refined sealed type with type member") {
    testRoundtrip[SealedRefined {type X = Int}](SealedRefined.First(42))
  }

  test("recursive materialization of indirectly recursive type") {
    def testWithCodec(implicit codec: GenCodec[StepOne]): Unit = {
      testRoundtrip[StepOne](StepOne(StepTwo(Opt.Empty)))
      testRoundtrip[StepOne](StepOne(StepTwo(Opt(StepOne(StepTwo(Opt.Empty))))))
    }
    testWithCodec(GenCodec.materializeRecursively)
    testWithCodec {
      implicit val implCodec: GenCodec[StepOne] = GenCodec.materializeRecursively
      implCodec
    }
  }

  test("auto materialized key codec") {
    testRoundtrip[Map[ThingId, ThingId]](Map(ThingId("a") -> ThingId("b")))
  }

  test("Java builder based codec") {
    testRoundtrip[BuildablePojo](BuildablePojo.builder().build())
    testRoundtrip[BuildablePojo](BuildablePojo.builder().setStr("foo").build())
    testRoundtrip[BuildablePojo](BuildablePojo.builder().setStr("foo").setFlags(JList(true, false)).setCool(false).build())
  }
}
