package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.misc.{TypedKey, TypedKeyCompanion, TypedMap}
import com.github.ghik.silencer.silent

import scala.collection.immutable.ListMap

object GenCodecTest {
  case class ValueClass(str: String) extends AnyVal
  object ValueClass extends HasGenCodec[ValueClass]
}

sealed trait SealedBase
object SealedBase {
  case object CaseObject extends SealedBase
  case class CaseClass(str: String) extends SealedBase
  case class Rec(sub: Opt[SealedBase], local: Opt[Rec]) extends SealedBase

  sealed trait InnerBase extends SealedBase
  object InnerBase {
    case object InnerCaseObject extends InnerBase
    case class InnerCaseClass(str: String = "kek") extends InnerBase
  }

  implicit val codec: GenCodec[SealedBase] = GenCodec.materialize[SealedBase]
}

class mongoId extends AnnotationAggregate {
  @outOfOrder
  @name("_id")
  final def aggregated: List[StaticAnnotation] = reifyAggregated
}

@flatten sealed trait FlatSealedBase {
  @mongoId def id: String
  @generated @name("upper_id") def upperId: String = id.toUpperCase
}
object FlatSealedBase extends HasGenCodec[FlatSealedBase] {
  case class FirstCase(id: String, int: Int = 42) extends FlatSealedBase
  case class SecondCase(id: String, dbl: Double, moar: Double*) extends FlatSealedBase
  case object ThirdCase extends FlatSealedBase {
    @generated def id = "third"
  }
  case class RecursiveCase(id: String, sub: Opt[FlatSealedBase]) extends FlatSealedBase
  case class LocallyRecursiveCase(id: String, sub: Opt[LocallyRecursiveCase]) extends FlatSealedBase
}

abstract class Wrapper[Self <: Wrapper[Self] : ClassTag](private val args: Any*) { this: Self =>
  override def equals(obj: Any): Boolean = obj match {
    case other: Self => args == other.args
    case _ => false
  }
  override def hashCode(): Int = args.hashCode()
}

trait Framework {
  type Field

  case class Stuff(field: Field)
}

trait BetterFramework extends Framework {
  implicit def fieldCodec: GenCodec[Field]

  implicit val stuffCodec: GenCodec[Stuff] = GenCodec.materialize
}

@silent
class GenCodecTest extends CodecTestBase {

  import GenCodecTest._

  test("java collection test") {
    testWriteRead[JCollection[Int]](jArrayList, List(1, 2, 3))
    testWriteRead[JList[Int]](jArrayList, List(1, 2, 3))
    testWriteRead[JArrayList[Int]](jArrayList, List(1, 2, 3))
    testWriteRead[JLinkedList[Int]](jLinkedList, List(1, 2, 3))
    testWriteRead[JSet[Int]](jHashSet, List(1, 2, 3))
    testWriteRead[JHashSet[Int]](jHashSet, List(1, 2, 3))
    testWriteRead[JLinkedHashSet[Int]](jLinkedHashSet, List(1, 2, 3))
    testWriteRead[JSortedSet[Int]](jTreeSet, List(1, 2, 3))
    testWriteRead[JNavigableSet[Int]](jTreeSet, List(1, 2, 3))
    testWriteRead[JTreeSet[Int]](jTreeSet, List(1, 2, 3))
    testWriteRead[JMap[String, Int]](jHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JHashMap[String, Int]](jHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JLinkedHashMap[String, Int]](jLinkedHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JHashMap[Int, Int]](jIntHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
  }

  test("NoState test") {
    type NoState = Nothing {type Dummy = Nothing}
    assert(implicitly[GenCodec[NoState]] == GenCodec.NothingCodec)
  }

  test("collections and wrappers test") {
    testWriteRead[Option[Int]](some, 42)
    testWriteRead[Option[Int]](none, null)
    testWriteRead[Either[Int, String]](Left(42), Map("Left" -> 42))
    testWriteRead[Either[Int, String]](Right("lol"), Map("Right" -> "lol"))
    testWriteRead[List[Int]](list, list)
    testWriteRead[Set[Int]](set, set.toList)
    testWriteRead[Map[String, Int]](map, map)
    testWriteRead[Map[Int, Int]](intMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[IHashMap[String, Int]](hashMap, hashMap)
  }

  test("tuple test") {
    testWriteRead((1, 2, 3),
      List(1, 2, 3))
    testWriteRead((1, "lol"),
      List(1, "lol"))
    testWriteRead((1, "lol", 3.0, 'a', List("dafuq", "fuu")),
      List(1, "lol", 3.0, "a", List("dafuq", "fuu"))
    )
  }

  object SomeObject {
    @generated def random: Int = 42
    implicit val codec: GenCodec[SomeObject.type] = GenCodec.materialize[SomeObject.type]
  }

  test("object test") {
    testWriteRead(SomeObject, Map("random" -> 42))
  }

  case class NoArgCaseClass()
  object NoArgCaseClass extends HasGenCodec[NoArgCaseClass]

  test("no arg case class test") {
    testWriteRead(NoArgCaseClass(), Map())
  }

  case class SingleArgCaseClass(str: String)
  object SingleArgCaseClass extends HasGenCodec[SingleArgCaseClass]

  test("single arg case class test") {
    testWriteRead(SingleArgCaseClass("something"), Map("str" -> "something"))
  }

  @transparent
  case class TransparentWrapper(str: String)
  object TransparentWrapper extends HasGenCodec[TransparentWrapper]

  test("transparent wrapper test") {
    testWriteRead(TransparentWrapper("something"), "something")
  }

  @transparent
  case class TransparentWrapperWithDependency(str: String)
  object TransparentWrapperWithDependency {
    //order matters
    implicit val codec: GenCodec[TransparentWrapperWithDependency] = GenCodec.materialize
    implicit val stringCodec: GenCodec[String] = GenCodec.StringCodec
  }

  test("transparent wrapper with dependency test") {
    testWriteRead(TransparentWrapperWithDependency("something"), "something")
  }

  @transparent case class StringId(id: String)
  object StringId extends TransparentWrapperCompanion[String, StringId]

  test("transparent wrapper companion test") {
    testWriteRead(StringId("lolfuu"), "lolfuu")
  }

  trait HasSomeStr {
    @name("some.str") def str: String
    @generated def someStrLen: Int = str.length
  }
  case class SomeCaseClass(str: String, intList: List[Int]) extends HasSomeStr
  object SomeCaseClass extends HasGenCodec[SomeCaseClass]

  test("case class test") {
    testWriteRead(SomeCaseClass("dafuq", List(1, 2, 3)),
      Map("some.str" -> "dafuq", "intList" -> List(1, 2, 3), "someStrLen" -> 5)
    )
  }

  case class Stuff[T](name: String)
  object Stuff {
    implicit val codec: GenCodec[Stuff[_]] = GenCodec.create(
      in => new Stuff[Any](in.readSimple().readString()),
      (out, value) => out.writeSimple().writeString(value.name)
    )
  }
  case class CaseClassWithWildcard(stuff: Stuff[_])
  object CaseClassWithWildcard extends HasGenCodec[CaseClassWithWildcard]

  test("case class with wildcard test") {
    testWriteRead(CaseClassWithWildcard(Stuff("lol")), Map("stuff" -> "lol"))
  }

  class CaseClassLike(val str: String, val intList: List[Int])
    extends Wrapper[CaseClassLike](str, intList)
  object CaseClassLike extends HasGenCodec[CaseClassLike] {
    def apply(@name("some.str") str: String, intList: List[Int]): CaseClassLike = new CaseClassLike(str, intList)
    def unapply(ccl: CaseClassLike): Opt[(String, List[Int])] = (ccl.str, ccl.intList).opt
  }

  test("case class like test") {
    testWriteRead(CaseClassLike("dafuq", List(1, 2, 3)),
      Map("some.str" -> "dafuq", "intList" -> List(1, 2, 3))
    )
  }

  class HasInheritedApply(val str: String, val intList: List[Int])
    extends Wrapper[HasInheritedApply](str, intList)
  trait ApplyAndUnapply[A, B, C] {
    protected def doApply(a: A, lb: List[B]): C
    protected def doUnapply(c: C): Option[(A, List[B])]
    def apply(a: A, lb: List[B]): C = doApply(a, lb)
    def unapply(c: C): Option[(A, List[B])] = doUnapply(c)
  }
  object HasInheritedApply extends HasGenCodec[HasInheritedApply] with ApplyAndUnapply[String, Int, HasInheritedApply] {
    protected def doApply(a: String, lb: List[Int]): HasInheritedApply = new HasInheritedApply(a, lb)
    protected def doUnapply(c: HasInheritedApply): Option[(String, List[Int])] = (c.str, c.intList).option
  }

  test("case class like with inherited apply/unapply test") {
    testWriteRead(HasInheritedApply("dafuq", List(1, 2, 3)),
      Map("a" -> "dafuq", "lb" -> List(1, 2, 3))
    )
  }

  case class ThirdParty(i: Int, s: String)
  object ThirdParty extends HasGenCodecFromAU[ThirdPartyFakeCompanion.type, ThirdParty]

  object ThirdPartyFakeCompanion {
    def apply(str: String, int: Int): ThirdParty = ThirdParty(int, str)
    def unapply(tp: ThirdParty): Opt[(String, Int)] = (tp.s, tp.i).opt
  }

  test("apply/unapply provider based codec test") {
    testWriteRead(ThirdParty(42, "lol"),
      Map("str" -> "lol", "int" -> 42)
    )
  }

  case class VarargsCaseClass(int: Int, strings: String*)
  object VarargsCaseClass extends HasGenCodec[VarargsCaseClass]

  case class OnlyVarargsCaseClass(strings: String*)
  object OnlyVarargsCaseClass extends HasGenCodec[OnlyVarargsCaseClass]

  test("varargs case class test") {
    testWriteRead(VarargsCaseClass(42, "foo", "bar"),
      Map("int" -> 42, "strings" -> List("foo", "bar"))
    )
  }

  test("only varargs case class test") {
    testWriteRead(OnlyVarargsCaseClass("42", "420"),
      Map("strings" -> List("42", "420"))
    )
  }

  class VarargsCaseClassLike(val str: String, val ints: Seq[Int]) extends Wrapper[VarargsCaseClassLike](str, ints)
  object VarargsCaseClassLike extends HasGenCodec[VarargsCaseClassLike] {
    def apply(@name("some.str") str: String, ints: Int*): VarargsCaseClassLike = new VarargsCaseClassLike(str, ints)
    def unapplySeq(vccl: VarargsCaseClassLike): Opt[(String, Seq[Int])] = (vccl.str, vccl.ints).opt
  }

  class OnlyVarargsCaseClassLike(val strings: Seq[String]) extends Wrapper[OnlyVarargsCaseClassLike](strings)
  object OnlyVarargsCaseClassLike extends HasGenCodec[OnlyVarargsCaseClassLike] {
    def apply(strings: String*): OnlyVarargsCaseClassLike = new OnlyVarargsCaseClassLike(strings)
    def unapplySeq(vccl: OnlyVarargsCaseClassLike): Opt[Seq[String]] = vccl.strings.opt
  }

  test("varargs case class like test") {
    testWriteRead(VarargsCaseClassLike("dafuq", 1, 2, 3),
      Map("some.str" -> "dafuq", "ints" -> List(1, 2, 3))
    )
  }

  test("only varargs case class like test") {
    testWriteRead(OnlyVarargsCaseClassLike("dafuq", "indeed"),
      Map("strings" -> List("dafuq", "indeed"))
    )
  }

  case class HasDefaults(@transientDefault int: Int = 42, @transientDefault @whenAbsent("dafuq") str: String = "kek")
  object HasDefaults extends HasGenCodec[HasDefaults]

  test("case class with default values test") {
    testWriteRead(HasDefaults(str = "lol"), Map("str" -> "lol"))
    testWriteRead(HasDefaults(43, "lol"), Map("int" -> 43, "str" -> "lol"))
    testWriteRead(HasDefaults(str = null), Map("str" -> null))
    testWriteRead(HasDefaults(str = "dafuq"), Map())
  }

  case class Node[T](value: T, children: List[Node[T]] = Nil)
  object Node extends HasPolyGenCodec[Node]

  test("recursive generic case class test") {
    testWriteRead(
      Node(123, List(
        Node(42, List(
          Node(52),
          Node(53)
        )),
        Node(43)
      )),
      Map[String, Any]("value" -> 123, "children" -> List(
        Map[String, Any]("value" -> 42, "children" -> List(
          Map[String, Any]("value" -> 52, "children" -> List()),
          Map[String, Any]("value" -> 53, "children" -> List())
        )),
        Map[String, Any]("value" -> 43, "children" -> List())
      ))
    )
  }

  sealed trait CustomList
  case object CustomTail extends CustomList
  @transparent case class CustomCons(tail: CustomList) extends CustomList
  object CustomCons extends HasGenCodec[CustomCons]
  object CustomList extends HasGenCodec[CustomList]

  test("recursively defined sealed hierarchy with explicit case class codec test") {
    testWriteRead[CustomList](CustomTail, Map("CustomTail" -> Map()))
    testWriteRead[CustomList](CustomCons(CustomCons(CustomTail)),
      Map("CustomCons" -> Map("CustomCons" -> Map("CustomTail" -> Map()))))
  }

  test("value class test") {
    testWriteRead(ValueClass("costam"), Map("str" -> "costam"))
  }

  test("sealed hierarchy test") {
    testWriteRead[SealedBase](SealedBase.CaseObject,
      Map("CaseObject" -> Map()))
    testWriteRead[SealedBase](SealedBase.CaseClass("fuu"),
      Map("CaseClass" -> Map("str" -> "fuu")))
    testWriteRead[SealedBase](SealedBase.InnerBase.InnerCaseObject,
      Map("InnerCaseObject" -> Map()))
    testWriteRead[SealedBase](SealedBase.InnerBase.InnerCaseClass("fuu"),
      Map("InnerCaseClass" -> Map("str" -> "fuu")))
  }

  test("flat sealed hierarchy test") {
    testWriteRead[FlatSealedBase](FlatSealedBase.FirstCase("fuu", 42),
      Map("_case" -> "FirstCase", "_id" -> "fuu", "int" -> 42, "upper_id" -> "FUU"))
    testWriteRead[FlatSealedBase](FlatSealedBase.SecondCase("bar", 3.14, 1.0, 2.0),
      Map("_case" -> "SecondCase", "_id" -> "bar", "dbl" -> 3.14, "moar" -> List(1.0, 2.0), "upper_id" -> "BAR"))
    testWriteRead[FlatSealedBase](FlatSealedBase.ThirdCase,
      Map("_case" -> "ThirdCase", "_id" -> "third", "upper_id" -> "THIRD"))
    testWriteRead[FlatSealedBase](FlatSealedBase.RecursiveCase("rec", Opt(FlatSealedBase.ThirdCase)),
      Map("_case" -> "RecursiveCase", "_id" -> "rec", "upper_id" -> "REC", "sub" ->
        Map("_case" -> "ThirdCase", "_id" -> "third", "upper_id" -> "THIRD")))
  }

  test("random field access dependent flat sealed hierarchy reading test") {
    testRead[FlatSealedBase](
      ListMap("_id" -> "fuu", "int" -> 42, "upper_id" -> "FUU", "_case" -> "FirstCase"),
      FlatSealedBase.FirstCase("fuu", 42))
  }

  test("out of order field in flat sealed hierarchy test") {
    testRead[FlatSealedBase](
      Map("_id" -> "fuu", "upper_id" -> "FUU", "random" -> 13, "_case" -> "FirstCase", "int" -> 42),
      FlatSealedBase.FirstCase("fuu", 42))
    testRead[FlatSealedBase](
      Map("_id" -> "bar", "upper_id" -> "FUU", "random" -> 13, "_case" -> "SecondCase", "dbl" -> 3.14, "moar" -> List(1.0, 2.0)),
      FlatSealedBase.SecondCase("bar", 3.14, 1.0, 2.0))
  }

  sealed trait BaseExpr {
    type Value
    def value: Value
  }
  sealed abstract class Expr[T](val value: T) extends BaseExpr {
    type Value = T
  }
  case class IntExpr(int: Int) extends Expr[Int](int)
  case class StringExpr(str: String) extends Expr[String](str)
  case object NullExpr extends Expr[Null](null)
  object BaseExpr {
    implicit val baseCodec: GenCodec[BaseExpr] = GenCodec.materialize[BaseExpr]
    implicit val codec: GenCodec[Expr[_]] = GenCodec.materialize[Expr[_]]
    implicit val stringCodec: GenCodec[Expr[String]] = GenCodec.materialize[Expr[String]]
  }

  test("GADT test") {
    testWriteRead[Expr[_]](NullExpr, Map("NullExpr" -> Map()))
    testWriteRead[Expr[_]](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
    testWriteRead[Expr[String]](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
    testWriteRead[BaseExpr](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
  }

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  object Tree extends HasPolyGenCodec[Tree]

  // test type dealiasing during materialization
  type IntTree = Tree[Int]
  GenCodec.materialize[IntTree]
  type IntBranch = Branch[Int]
  GenCodec.materialize[IntBranch]

  test("recursive generic ADT test") {
    testWriteRead[Tree[Int]](
      Branch(
        Leaf(1),
        Branch(
          Leaf(2),
          Leaf(3)
        )
      ),
      Map("Branch" -> Map(
        "left" -> Map("Leaf" -> Map("value" -> 1)),
        "right" -> Map("Branch" -> Map(
          "left" -> Map("Leaf" -> Map("value" -> 2)),
          "right" -> Map("Leaf" -> Map("value" -> 3))
        ))
      ))
    )
  }

  sealed trait Enumz
  object Enumz {
    @name("Primary")
    case object First extends Enumz
    case object Second extends Enumz
    case object Third extends Enumz

    implicit val codec: GenCodec[Enumz] = GenCodec.materialize[Enumz]
  }

  test("sealed enum test") {
    testWriteRead[Enumz](Enumz.First, Map("Primary" -> Map()))
    testWriteRead[Enumz](Enumz.Second, Map("Second" -> Map()))
    testWriteRead[Enumz](Enumz.Third, Map("Third" -> Map()))
  }

  sealed trait KeyEnumz
  object KeyEnumz {
    @name("Primary")
    case object First extends KeyEnumz
    case object Second extends KeyEnumz
    case object Third extends KeyEnumz

    implicit val codec: GenCodec[KeyEnumz] = GenCodec.forSealedEnum[KeyEnumz]
  }

  test("sealed enum based on key codec test") {
    testWriteRead[KeyEnumz](KeyEnumz.First, "Primary")
    testWriteRead[KeyEnumz](KeyEnumz.Second, "Second")
    testWriteRead[KeyEnumz](KeyEnumz.Third, "Third")
  }

  sealed abstract class SealedKey[T: GenCodec] extends TypedKey[T]
  object SealedKey extends TypedKeyCompanion[SealedKey] {
    @name("StrKey")
    case object StringKey extends SealedKey[String]
    case object IntKey extends SealedKey[Int]
    case object BooleanKey extends SealedKey[Boolean]

    val values: List[SealedKey[_]] = caseObjects
    implicit def keyCodec: GenKeyCodec[SealedKey[_]] = GenKeyCodec.forSealedEnum[SealedKey[_]]
  }

  test("typed map test") {
    import SealedKey._

    testWriteRead(
      TypedMap(StringKey -> "lol", IntKey -> 42, BooleanKey -> true),
      Map[String, Any]("StrKey" -> "lol", "IntKey" -> 42, "BooleanKey" -> true)
    )
  }

  @flatten("kejs") sealed trait CustomizedSeal
  @defaultCase(transient = true) case class CustomizedCase(str: String) extends CustomizedSeal
  case object CustomizedObjekt extends CustomizedSeal
  object CustomizedSeal extends HasGenCodec[CustomizedSeal]

  test("customized flat sealed hierarchy test") {
    testWriteRead[CustomizedSeal](CustomizedCase("dafuq"),
      Map("str" -> "dafuq"))
    testWriteRead[CustomizedSeal](CustomizedObjekt,
      Map("kejs" -> "CustomizedObjekt"))
  }

  case class ItsOverTwentyTwo(
    a1: String,
    a2: String,
    a3: String,
    a4: String,
    a5: String,
    a6: String,
    a7: String,
    a8: String,
    a9: String,
    a10: String,
    a11: String,
    a12: String,
    a13: String,
    a14: String,
    a15: String,
    a16: String,
    a17: String,
    a18: String,
    a19: String,
    a20: String,
    a21: String,
    a22: String,
    a23: String
  )
  object ItsOverTwentyTwo extends HasGenCodec[ItsOverTwentyTwo]

  test("case class with more than 22 fields") {
    val inst = ItsOverTwentyTwo(
      "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10",
      "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20",
      "v21", "v22", "v23")
    val repr = (1 to 23).map(i => s"a$i" -> s"v$i").toMap
    testWriteRead[ItsOverTwentyTwo](inst, repr)
  }

  @flatten
  sealed trait Dep
  case class DepCase(str: String) extends Dep

  @flatten
  sealed trait HasColl
  case class HasCollCase(coll: Seq[Dep]) extends HasColl
  object HasColl extends HasRecursiveGenCodec[HasColl]

  test("recursive materialization with intermediate sequence") {
    testWriteRead[HasColl](
      HasCollCase(List(DepCase("kek"))),
      Map("_case" -> "HasCollCase", "coll" -> List(Map("_case" -> "DepCase", "str" -> "kek")))
    )
  }

  sealed trait SealedRefined {
    type X
  }
  object SealedRefined {
    final case class First[Type](foo: Type) extends SealedRefined {
      type X = Type
    }
    implicit def codec[T: GenCodec]: GenCodec[SealedRefined {type X = T}] = GenCodec.materialize
  }

  test("refined sealed type with type member") {
    testWriteRead[SealedRefined {type X = Int}](
      SealedRefined.First(42),
      Map("First" -> Map("foo" -> 42))
    )
  }

  case class StepOne(stepTwo: StepTwo)
  case class StepTwo(stepOne: Opt[StepOne])

  test("recursive materialization of indirectly recursive type") {
    def testWithCodec(implicit codec: GenCodec[StepOne]): Unit = {
      testWriteRead[StepOne](StepOne(StepTwo(Opt.Empty)),
        Map("stepTwo" -> Map("stepOne" -> null))
      )
      testWriteRead[StepOne](StepOne(StepTwo(Opt(StepOne(StepTwo(Opt.Empty))))),
        Map("stepTwo" -> Map("stepOne" -> Map("stepTwo" -> Map("stepOne" -> null))))
      )
    }
    testWithCodec(GenCodec.materializeRecursively)
    testWithCodec {
      implicit val implCodec: GenCodec[StepOne] = GenCodec.materializeRecursively
      implCodec
    }
  }

  case class OuterThing(inner: InnerThing)
  case class InnerThing(recursiveThing: Opt[OuterThing])
  object OuterThing extends HasRecursiveGenCodec[OuterThing]

  @transparent
  case class ThingId(value: String)
  object ThingId extends HasGenAndKeyCodec[ThingId]

  test("auto materialized key codec") {
    testWriteRead[Map[ThingId, ThingId]](Map(ThingId("a") -> ThingId("b")), Map("a" -> "b"))
  }
}
