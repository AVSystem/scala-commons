package com.avsystem.commons
package serialization

import java.lang.annotation.RetentionPolicy

import com.avsystem.commons.misc.{TypedKey, TypedKeyCompanion, TypedMap}
import com.avsystem.commons.serialization.GenCodecTest.ValueClass
import com.github.ghik.silencer.silent

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
    testWriteReadAndAutoWriteRead[Map[Int, Int]](intMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[Map[Double, Int]](doubleMap,
      List(Map[String, Any]("k" -> 1.0, "v" -> 1), Map[String, Any]("k" -> 2.0, "v" -> 2), Map[String, Any]("k" -> 3.0, "v" -> 3)))
    testWriteReadAndAutoWriteRead[IHashMap[String, Int]](hashMap, hashMap)
  }

  test("java colleciton test") {
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
    testWriteReadAndAutoWriteRead[JMap[String, Int]](jHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JHashMap[String, Int]](jHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JLinkedHashMap[String, Int]](jLinkedHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JHashMap[Int, Int]](jIntHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JHashMap[Double, Int]](jDoubleHashMap,
      List(Map[String, Any]("k" -> 1.0, "v" -> 1), Map[String, Any]("k" -> 2.0, "v" -> 2), Map[String, Any]("k" -> 3.0, "v" -> 3))
    )
    testWriteReadAndAutoWriteRead[JSortedMap[String, Int]](jTreeMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JNavigableMap[String, Int]](jTreeMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JTreeMap[String, Int]](jTreeMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
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
    implicit val codec = GenCodec.materialize[SomeObject.type]
  }

  test("object test") {
    testWriteReadAndAutoWriteRead(SomeObject, Map())
  }

  case class NoArgCaseClass()
  object NoArgCaseClass extends HasGenCodec[NoArgCaseClass]

  test("no arg case class test") {
    testWriteReadAndAutoWriteRead(NoArgCaseClass(), Map())
  }

  case class SingleArgCaseClass(str: String)
  object SingleArgCaseClass extends HasGenCodec[SingleArgCaseClass]

  test("single arg case class test") {
    testWriteReadAndAutoWriteRead(SingleArgCaseClass("something"), Map("str" -> "something"))
  }

  @transparent
  case class TransparentWrapper(str: String)
  object TransparentWrapper extends HasGenCodec[TransparentWrapper]

  test("transparent wrapper test") {
    testWriteReadAndAutoWriteRead(TransparentWrapper("something"), "something")
  }

  case class SomeCaseClass(@name("some.str") str: String, intList: List[Int])
  object SomeCaseClass extends HasGenCodec[SomeCaseClass]

  test("case class test") {
    testWriteReadAndAutoWriteRead(SomeCaseClass("dafuq", List(1, 2, 3)),
      Map("some.str" -> "dafuq", "intList" -> List(1, 2, 3))
    )
  }

  case class HasDefaults(@transientDefault int: Int = 42, str: String)
  object HasDefaults {
    implicit val codec = GenCodec.materialize[HasDefaults]
  }

  test("case class with default values test") {
    testWriteReadAndAutoWriteRead(HasDefaults(str = "lol"), Map("str" -> "lol"))
    testWriteReadAndAutoWriteRead(HasDefaults(43, "lol"), Map("int" -> 43, "str" -> "lol"))
    testWriteReadAndAutoWriteRead(HasDefaults(str = null), Map("str" -> null))
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
    testWriteReadAndAutoWriteRead[CustomList](CustomTail, Map("CustomTail" -> Map()))
    testWriteReadAndAutoWriteRead[CustomList](CustomCons(CustomCons(CustomTail)),
      Map("CustomCons" -> Map("CustomCons" -> Map("CustomTail" -> Map()))))
  }

  test("value class test") {
    testWriteReadAndAutoWriteRead(ValueClass("costam"), Map("str" -> "costam"))
  }

  test("sealed hierarchy test") {
    testWriteReadAndAutoWriteRead[SealedBase](SealedBase.CaseObject, Map("CaseObject" -> Map()))
    testWriteReadAndAutoWriteRead[SealedBase](SealedBase.CaseClass("fuu"), Map("CaseClass" -> Map("str" -> "fuu")))
    testWriteReadAndAutoWriteRead[SealedBase](SealedBase.InnerBase.InnerCaseObject, Map("InnerCaseObject" -> Map()))
    testWriteReadAndAutoWriteRead[SealedBase](SealedBase.InnerBase.InnerCaseClass("fuu"), Map("InnerCaseClass" -> Map("str" -> "fuu")))
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
    testWriteReadAndAutoWriteRead[Expr[_]](NullExpr, Map("NullExpr" -> Map()))
    testWriteReadAndAutoWriteRead[Expr[_]](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
    testWriteReadAndAutoWriteRead[Expr[String]](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
    testWriteReadAndAutoWriteRead[BaseExpr](StringExpr("stringzor"), Map("StringExpr" -> Map("str" -> "stringzor")))
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
    testWriteReadAndAutoWriteRead[Enumz](Enumz.First, Map("Primary" -> Map()))
    testWriteReadAndAutoWriteRead[Enumz](Enumz.Second, Map("Second" -> Map()))
    testWriteReadAndAutoWriteRead[Enumz](Enumz.Third, Map("Third" -> Map()))
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
