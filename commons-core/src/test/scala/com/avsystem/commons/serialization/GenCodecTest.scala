package com.avsystem.commons
package serialization

import java.lang.annotation.RetentionPolicy

import com.avsystem.commons.misc.{TypedKey, TypedKeyCompanion, TypedMap}
import com.avsystem.commons.serialization.GenCodecTest.ValueClass
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

  sealed trait InnerBase extends SealedBase
  object InnerBase {
    case object InnerCaseObject extends InnerBase
    case class InnerCaseClass(str: String) extends InnerBase
  }

  @silent //exhaustivity checker has some problems with macro-generated match on doubly nested inner case classes/objects
  implicit val codec: GenCodec[SealedBase] = GenCodec.materialize[SealedBase]
}

@flatten sealed trait FlatSealedBase {
  @outOfOrder
  @name("_id") def id: String
  @generated
  @name("upper_id") def upperId: String = id.toUpperCase
}
object FlatSealedBase {
  case class FirstCase(id: String, int: Int) extends FlatSealedBase
  case class SecondCase(id: String, dbl: Double, moar: Double*) extends FlatSealedBase
  case object ThirdCase extends FlatSealedBase {
    @generated def id = "third"
  }

  implicit val codec: GenCodec[FlatSealedBase] = GenCodec.materialize[FlatSealedBase]
}

abstract class Wrapper[Self <: Wrapper[Self] : ClassTag](private val args: Any*) { this: Self =>
  override def equals(obj: Any): Boolean = obj match {
    case other: Self => args == other.args
    case _ => false
  }
  override def hashCode(): Int = args.hashCode()
}

@silent
class GenCodecTest extends CodecTestBase {
  test("NoState test") {
    type NoState = Nothing {type Dummy = Nothing}
    assert(implicitly[GenCodec[NoState]] == GenCodec.NothingCodec)
  }

  test("collection test") {
    testWriteReadAndAutoWriteRead[Option[Int]](option, List(42))
    testWriteReadAndAutoWriteRead[List[Int]](list, list)
    testWriteReadAndAutoWriteRead[Set[Int]](set, set.toList)
    testWriteReadAndAutoWriteRead[Map[String, Int]](map, map)
    testWriteReadAndAutoWriteRead[Map[Int, Int]](intMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[Map[Double, Int]](doubleMap,
      List(Map[String, Any]("k" -> 1.0, "v" -> 1), Map[String, Any]("k" -> 2.0, "v" -> 2), Map[String, Any]("k" -> 3.0, "v" -> 3)))
    testWriteReadAndAutoWriteRead[IHashMap[String, Int]](hashMap, hashMap)
  }

  test("java collection test") {
    testWriteReadAndAutoWriteRead[JCollection[Int]](jArrayList, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JList[Int]](jArrayList, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JArrayList[Int]](jArrayList, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JLinkedList[Int]](jLinkedList, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JSet[Int]](jHashSet, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JHashSet[Int]](jHashSet, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JLinkedHashSet[Int]](jLinkedHashSet, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JSortedSet[Int]](jTreeSet, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JNavigableSet[Int]](jTreeSet, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JTreeSet[Int]](jTreeSet, List(1, 2, 3))
    testWriteReadAndAutoWriteRead[JMap[String, Int]](jHashMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JHashMap[String, Int]](jHashMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JLinkedHashMap[String, Int]](jLinkedHashMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JHashMap[Int, Int]](jIntHashMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JHashMap[Double, Int]](jDoubleHashMap,
      List(Map[String, Any]("k" -> 1.0, "v" -> 1), Map[String, Any]("k" -> 2.0, "v" -> 2), Map[String, Any]("k" -> 3.0, "v" -> 3))
    )
    testWriteReadAndAutoWriteRead[JSortedMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JNavigableMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JTreeMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
  }

  test("tuple test") {
    testWriteReadAndAutoWriteRead((1, 2, 3),
      List(1, 2, 3))
    testWriteReadAndAutoWriteRead((1, "lol"),
      List(1, "lol"))
    testWriteReadAndAutoWriteRead((1, "lol", 3.0, 'a', List("dafuq", "fuu")),
      List(1, "lol", 3.0, "a", List("dafuq", "fuu"))
    )
  }

  test("java enum test") {
    testWriteReadAndAutoWriteRead(RetentionPolicy.RUNTIME, "RUNTIME")
    testWriteReadAndAutoWriteRead(RetentionPolicy.SOURCE, "SOURCE")
  }

  object SomeObject {
    @generated def random: Int = 42
    implicit val codec: GenCodec[SomeObject.type] = GenCodec.materialize[SomeObject.type]
  }

  test("object test") {
    testWriteReadAndAutoWriteRead(SomeObject, Map("random" -> 42))
  }

  case class NoArgCaseClass()
  object NoArgCaseClass extends HasGenCodec[NoArgCaseClass]

  test("no arg case class test") {
    testWriteReadAndAutoWriteRead(NoArgCaseClass(), Map())
  }

  case class SingleArgCaseClass(str: String)
  object SingleArgCaseClass extends HasGenCodec[SingleArgCaseClass]

  test("single arg case class test") {
    testWriteReadAndAutoWriteRead(SingleArgCaseClass("something"), ListMap("str" -> "something"))
  }

  @transparent
  case class TransparentWrapper(str: String)
  object TransparentWrapper extends HasGenCodec[TransparentWrapper]

  test("transparent wrapper test") {
    testWriteReadAndAutoWriteRead(TransparentWrapper("something"), "something")
  }

  trait HasSomeStr {
    @name("some.str") def str: String
    @generated def someStrLen: Int = str.length
  }
  case class SomeCaseClass(str: String, intList: List[Int]) extends HasSomeStr
  object SomeCaseClass extends HasGenCodec[SomeCaseClass]

  test("case class test") {
    testWriteReadAndAutoWriteRead(SomeCaseClass("dafuq", List(1, 2, 3)),
      ListMap("some.str" -> "dafuq", "intList" -> List(1, 2, 3), "someStrLen" -> 5)
    )
  }

  case class Stuff[T](name: String)
  object Stuff {
    implicit val codec: GenCodec[Stuff[_]] = GenCodec.create(
      in => new Stuff[Any](in.readString()),
      (out, value) => out.writeString(value.name)
    )
  }
  case class CaseClassWithWildcard(stuff: Stuff[_])
  object CaseClassWithWildcard {
    implicit val codec: GenCodec[CaseClassWithWildcard] = GenCodec.materialize[CaseClassWithWildcard]
  }

  test("case class with wildcard test") {
    testWriteReadAndAutoWriteRead(CaseClassWithWildcard(Stuff("lol")), ListMap("stuff" -> "lol"))
  }

  class CaseClassLike(val str: String, val intList: List[Int])
    extends Wrapper[CaseClassLike](str, intList)
  object CaseClassLike {
    def apply(@name("some.str") str: String, intList: List[Int]): CaseClassLike = new CaseClassLike(str, intList)
    def unapply(ccl: CaseClassLike): Opt[(String, List[Int])] = (ccl.str, ccl.intList).opt
    implicit val codec: GenCodec[CaseClassLike] = GenCodec.materialize[CaseClassLike]
  }

  test("case class like test") {
    testWriteReadAndAutoWriteRead(CaseClassLike("dafuq", List(1, 2, 3)),
      ListMap("some.str" -> "dafuq", "intList" -> List(1, 2, 3))
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
  object HasInheritedApply extends ApplyAndUnapply[String, Int, HasInheritedApply] {
    protected def doApply(a: String, lb: List[Int]): HasInheritedApply = new HasInheritedApply(a, lb)
    protected def doUnapply(c: HasInheritedApply): Option[(String, List[Int])] = (c.str, c.intList).option
    implicit val codec: GenCodec[HasInheritedApply] = GenCodec.materialize[HasInheritedApply]
  }

  test("case class like with inherited apply/unapply test") {
    testWriteReadAndAutoWriteRead(HasInheritedApply("dafuq", List(1, 2, 3)),
      ListMap("a" -> "dafuq", "lb" -> List(1, 2, 3))
    )
  }

  case class ThirdParty(i: Int, s: String)
  object ThirdPartyFakeCompanion {
    def apply(str: String, int: Int): ThirdParty = ThirdParty(int, str)
    def unapply(tp: ThirdParty): Opt[(String, Int)] = (tp.s, tp.i).opt
  }

  test("apply/unapply provider based codec test") {
    implicit val tpCodec: GenCodec[ThirdParty] = GenCodec.fromApplyUnapplyProvider[ThirdParty](ThirdPartyFakeCompanion)
    testWriteReadAndAutoWriteRead(ThirdParty(42, "lol"),
      ListMap("str" -> "lol", "int" -> 42)
    )
  }

  case class VarargsCaseClass(int: Int, strings: String*)
  object VarargsCaseClass {
    implicit val codec: GenCodec[VarargsCaseClass] = GenCodec.materialize[VarargsCaseClass]
  }

  test("varargs case class test") {
    testWriteReadAndAutoWriteRead(VarargsCaseClass(42, "foo", "bar"),
      ListMap("int" -> 42, "strings" -> List("foo", "bar"))
    )
  }

  class VarargsCaseClassLike(val str: String, val ints: Seq[Int])
    extends Wrapper[VarargsCaseClassLike](str, ints)
  object VarargsCaseClassLike {
    def apply(@name("some.str") str: String, ints: Int*): VarargsCaseClassLike = new VarargsCaseClassLike(str, ints)
    def unapplySeq(vccl: VarargsCaseClassLike): Opt[(String, Seq[Int])] = (vccl.str, vccl.ints).opt
    implicit val codec: GenCodec[VarargsCaseClassLike] = GenCodec.materialize[VarargsCaseClassLike]
  }

  test("varargs case class like test") {
    testWriteReadAndAutoWriteRead(VarargsCaseClassLike("dafuq", 1, 2, 3),
      ListMap("some.str" -> "dafuq", "ints" -> List(1, 2, 3))
    )
  }

  case class HasDefaults(@transientDefault int: Int = 42, str: String)
  object HasDefaults {
    implicit val codec: GenCodec[HasDefaults] = GenCodec.materialize[HasDefaults]
  }

  test("case class with default values test") {
    testWriteReadAndAutoWriteRead(HasDefaults(str = "lol"), ListMap("str" -> "lol"))
    testWriteReadAndAutoWriteRead(HasDefaults(43, "lol"), ListMap("int" -> 43, "str" -> "lol"))
    testWriteReadAndAutoWriteRead(HasDefaults(str = null), ListMap("str" -> null))
  }

  case class Node[T](value: T, children: List[Node[T]] = Nil)
  object Node {
    implicit def codec[T: GenCodec]: GenCodec[Node[T]] = GenCodec.materialize[Node[T]]
  }

  test("recursive generic case class test") {
    testWriteReadAndAutoWriteRead(
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
  object CustomCons {
    implicit val codec: GenCodec[CustomCons] = GenCodec.materialize[CustomCons]
  }
  object CustomList {
    implicit val codec: GenCodec[CustomList] = GenCodec.materialize[CustomList]
  }

  test("recursively defined sealed hierarchy with explicit case class codec test") {
    testWriteReadAndAutoWriteRead[CustomList](CustomTail, ListMap("CustomTail" -> Map()))
    testWriteReadAndAutoWriteRead[CustomList](CustomCons(CustomCons(CustomTail)),
      ListMap("CustomCons" -> ListMap("CustomCons" -> ListMap("CustomTail" -> Map()))))
  }

  test("value class test") {
    testWriteReadAndAutoWriteRead(ValueClass("costam"), ListMap("str" -> "costam"))
  }

  test("sealed hierarchy test") {
    testWriteReadAndAutoWriteRead[SealedBase](SealedBase.CaseObject,
      ListMap("CaseObject" -> Map()))
    testWriteReadAndAutoWriteRead[SealedBase](SealedBase.CaseClass("fuu"),
      ListMap("CaseClass" -> ListMap("str" -> "fuu")))
    testWriteReadAndAutoWriteRead[SealedBase](SealedBase.InnerBase.InnerCaseObject,
      ListMap("InnerCaseObject" -> Map()))
    testWriteReadAndAutoWriteRead[SealedBase](SealedBase.InnerBase.InnerCaseClass("fuu"),
      ListMap("InnerCaseClass" -> ListMap("str" -> "fuu")))
  }

  test("flat sealed hierarchy test") {
    testWriteReadAndAutoWriteRead[FlatSealedBase](FlatSealedBase.FirstCase("fuu", 42),
      ListMap("_case" -> "FirstCase", "_id" -> "fuu", "int" -> 42, "upper_id" -> "FUU"))
    testWriteReadAndAutoWriteRead[FlatSealedBase](FlatSealedBase.SecondCase("bar", 3.14, 1.0, 2.0),
      ListMap("_case" -> "SecondCase", "_id" -> "bar", "dbl" -> 3.14, "moar" -> List(1.0, 2.0), "upper_id" -> "BAR"))
    testWriteReadAndAutoWriteRead[FlatSealedBase](FlatSealedBase.ThirdCase,
      ListMap("_case" -> "ThirdCase", "_id" -> "third", "upper_id" -> "THIRD"))
  }

  test("out of order field in flat sealed hierarchy test") {
    testReadAndAutoRead[FlatSealedBase](
      ListMap("_id" -> "fuu", "upper_id" -> "FUU", "random" -> 13, "_case" -> "FirstCase", "int" -> 42),
      FlatSealedBase.FirstCase("fuu", 42))
    testReadAndAutoRead[FlatSealedBase](
      ListMap("_id" -> "bar", "upper_id" -> "FUU", "random" -> 13, "_case" -> "SecondCase", "dbl" -> 3.14, "moar" -> List(1.0, 2.0)),
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
    testWriteReadAndAutoWriteRead[Expr[_]](NullExpr, ListMap("NullExpr" -> Map()))
    testWriteReadAndAutoWriteRead[Expr[_]](StringExpr("stringzor"), ListMap("StringExpr" -> ListMap("str" -> "stringzor")))
    testWriteReadAndAutoWriteRead[Expr[String]](StringExpr("stringzor"), ListMap("StringExpr" -> ListMap("str" -> "stringzor")))
    testWriteReadAndAutoWriteRead[BaseExpr](StringExpr("stringzor"), ListMap("StringExpr" -> ListMap("str" -> "stringzor")))
  }

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  object Tree {
    implicit def codec[A: GenCodec]: GenCodec[Tree[A]] = GenCodec.materialize[Tree[A]]
  }

  test("recursive generic ADT test") {
    testWriteReadAndAutoWriteRead[Tree[Int]](
      Branch(
        Leaf(1),
        Branch(
          Leaf(2),
          Leaf(3)
        )
      ),
      ListMap("Branch" -> Map(
        "left" -> ListMap("Leaf" -> ListMap("value" -> 1)),
        "right" -> ListMap("Branch" -> Map(
          "left" -> ListMap("Leaf" -> ListMap("value" -> 2)),
          "right" -> ListMap("Leaf" -> ListMap("value" -> 3))
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
    testWriteReadAndAutoWriteRead[Enumz](Enumz.First, ListMap("Primary" -> Map()))
    testWriteReadAndAutoWriteRead[Enumz](Enumz.Second, ListMap("Second" -> Map()))
    testWriteReadAndAutoWriteRead[Enumz](Enumz.Third, ListMap("Third" -> Map()))
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
    testWriteReadAndAutoWriteRead[KeyEnumz](KeyEnumz.First, "Primary")
    testWriteReadAndAutoWriteRead[KeyEnumz](KeyEnumz.Second, "Second")
    testWriteReadAndAutoWriteRead[KeyEnumz](KeyEnumz.Third, "Third")
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

    testWriteReadAndAutoWriteRead(
      TypedMap(StringKey -> "lol", IntKey -> 42, BooleanKey -> true),
      Map[String, Any]("StrKey" -> "lol", "IntKey" -> 42, "BooleanKey" -> true)
    )
  }
}
