package com.avsystem.commons
package serialization

import com.avsystem.commons.collection.CollectionAliases._
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.serialization.GenCodecTest.ValueClass

/**
  * Author: ghik
  * Created: 18/11/15.
  */
object GenCodecTest {
  case class ValueClass(str: String) extends AnyVal
  object ValueClass {
    implicit val codec: GenCodec[ValueClass] = GenCodec.auto[ValueClass]
  }
}

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

  object SomeObject {
    implicit val codec = GenCodec.auto[SomeObject.type]
  }

  test("object test") {
    testWriteReadAndAutoWriteRead(SomeObject, Map())
  }

  case class NoArgCaseClass()
  object NoArgCaseClass {
    implicit val codec = GenCodec.auto[NoArgCaseClass]
  }

  test("no arg case class test") {
    testWriteReadAndAutoWriteRead(NoArgCaseClass(), Map())
  }

  case class SingleArgCaseClass(str: String)
  object SingleArgCaseClass {
    implicit val codec = GenCodec.auto[SingleArgCaseClass]
  }

  test("single arg case class test") {
    testWriteReadAndAutoWriteRead(SingleArgCaseClass("something"), Map("str" -> "something"))
  }

  @transparent
  case class TransparentWrapper(str: String)
  object TransparentWrapper {
    implicit val codec = GenCodec.auto[TransparentWrapper]
  }

  test("transparent wrapper test") {
    testWriteReadAndAutoWriteRead(TransparentWrapper("something"), "something")
  }

  case class SomeCaseClass(@name("some.str") str: String, intList: List[Int])
  object SomeCaseClass {
    implicit val codec = GenCodec.auto[SomeCaseClass]
  }

  test("case class test") {
    testWriteReadAndAutoWriteRead(SomeCaseClass("dafuq", List(1, 2, 3)),
      Map("some.str" -> "dafuq", "intList" -> List(1, 2, 3))
    )
  }

  case class HasDefaults(@transientDefault int: Int = 42, str: String)
  object HasDefaults {
    implicit val codec = GenCodec.auto[HasDefaults]
  }

  test("case class with default values test") {
    testWriteReadAndAutoWriteRead(HasDefaults(str = "lol"), Map("str" -> "lol"))
    testWriteReadAndAutoWriteRead(HasDefaults(43, "lol"), Map("int" -> 43, "str" -> "lol"))
    testWriteReadAndAutoWriteRead(HasDefaults(str = null), Map("str" -> null))
  }

  case class Node[T](value: T, children: List[Node[T]] = Nil)
  object Node {
    implicit def codec[T: GenCodec]: GenCodec[Node[T]] = GenCodec.auto[Node[T]]
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

  test("value class test") {
    testWriteReadAndAutoWriteRead(ValueClass("costam"), Map("str" -> "costam"))
  }

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  object Tree {
    implicit def codec[T: GenCodec]: GenCodec[Tree[T]] = GenCodec.auto[Tree[T]]
  }

  test("recursive gadt test") {
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

    implicit val codec: GenCodec[Enumz] = GenCodec.auto[Enumz]
  }

  test("sealed enum test") {
    testWriteReadAndAutoWriteRead[Enumz](Enumz.First, Map("Primary" -> Map()))
  }

  case class HasOperator(str: String, op: Operator[_])
  object HasOperator {
    implicit val codec: GenCodec[HasOperator] = GenCodec.recursiveAuto[HasOperator]
  }
  sealed trait Operator[T]
  case object StringOperator extends Operator[String]
  case object IntOperator extends Operator[Int]
}
