package com.avsystem.commons
package serialization

import com.avsystem.commons.collection.CollectionAliases._
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.serialization.GenCodecTest.ValueClass
import org.scalatest.FunSuite

/**
  * Author: ghik
  * Created: 18/11/15.
  */
object GenCodecTest {
  case class ValueClass(str: String) extends AnyVal
  object ValueClass {
    implicit val codec = GenCodec.auto[ValueClass]
  }
}

class GenCodecTest extends FunSuite {
  def assertSameTypeValue[T](v1: T, v2: T): Unit = {
    assert(v1 == v2)
    assert(v1.getClass == v2.getClass)
  }

  def col[T <: JCollection[Int]](col: T): T = {
    col.add(1)
    col.add(2)
    col.add(3)
    col
  }

  def map[M <: JMap[Int, Int]](map: M): M = {
    map.put(1, 1)
    map.put(2, 2)
    map.put(3, 3)
    map
  }

  def stringMap[M <: JMap[String, Int]](map: M): M = {
    map.put("1", 1)
    map.put("2", 2)
    map.put("3", 3)
    map
  }

  def doubleMap[M <: JMap[Double, Int]](map: M): M = {
    map.put(1.0, 1)
    map.put(2.0, 2)
    map.put(3.0, 3)
    map
  }

  val option = Option(42)
  val list = List(1, 2, 3)
  val set = Set(1, 2, 3)
  val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
  val hashMap = IHashMap("1" -> 1, "2" -> 2, "3" -> 3)
  val intMap = Map(1 -> 1, 2 -> 2, 3 -> 3)
  val doubleMap = Map(1.0 -> 1, 2.0 -> 2, 3.0 -> 3)

  val jArrayList = col(new JArrayList[Int])
  val jLinkedList = col(new JLinkedList[Int])
  val jHashSet = col(new JHashSet[Int])
  val jLinkedHashSet = col(new JLinkedHashSet[Int])
  val jTreeSet = col(new JTreeSet[Int])
  val jHashMap = stringMap(new JHashMap[String, Int])
  val jIntHashMap = map(new JHashMap[Int, Int])
  val jDoubleHashMap = doubleMap(new JHashMap[Double, Int])
  val jLinkedHashMap = stringMap(new JLinkedHashMap[String, Int])
  val jTreeMap = stringMap(new JTreeMap[String, Int])

  def testWriteRead[T: GenCodec](value: T, expectedRepr: Any): Unit = {
    var written: Any = null
    GenCodec.write[T](new SimpleValueOutput(written = _), value)
    assert(written == expectedRepr)
    val readBack = GenCodec.read[T](new SimpleValueInput(written))
    assertSameTypeValue(value, readBack)
  }

  test("NoState test") {
    type NoState = Nothing {type Dummy = Nothing}
    assert(implicitly[GenCodec[NoState]] == GenCodec.NothingCodec)
  }

  test("collection test") {
    testWriteRead[Option[Int]](option, List(42))
    testWriteRead[List[Int]](list, list)
    testWriteRead[Set[Int]](set, set.toList)
    testWriteRead[Map[String, Int]](map, map)
    testWriteRead[Map[Int, Int]](intMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[Map[Double, Int]](doubleMap,
      List(Map[String, Any]("k" -> 1.0, "v" -> 1), Map[String, Any]("k" -> 2.0, "v" -> 2), Map[String, Any]("k" -> 3.0, "v" -> 3)))
    testWriteRead[IHashMap[String, Int]](hashMap, hashMap)
  }

  test("java colleciton test") {
    testWriteRead[JCollection[Int]](jArrayList, List(1,2,3))
    testWriteRead[JList[Int]](jArrayList, List(1,2,3))
    testWriteRead[JArrayList[Int]](jArrayList, List(1,2,3))
    testWriteRead[JLinkedList[Int]](jLinkedList, List(1,2,3))
    testWriteRead[JSet[Int]](jHashSet, List(1,2,3))
    testWriteRead[JHashSet[Int]](jHashSet, List(1,2,3))
    testWriteRead[JLinkedHashSet[Int]](jLinkedHashSet, List(1,2,3))
    testWriteRead[JSortedSet[Int]](jTreeSet, List(1,2,3))
    testWriteRead[JNavigableSet[Int]](jTreeSet, List(1,2,3))
    testWriteRead[JTreeSet[Int]](jTreeSet, List(1,2,3))
    testWriteRead[JMap[String, Int]](jHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JHashMap[String, Int]](jHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JLinkedHashMap[String, Int]](jLinkedHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JHashMap[Int, Int]](jIntHashMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JHashMap[Double, Int]](jDoubleHashMap,
      List(Map[String, Any]("k" -> 1.0, "v" -> 1), Map[String, Any]("k" -> 2.0, "v" -> 2), Map[String, Any]("k" -> 3.0, "v" -> 3))
    )
    testWriteRead[JSortedMap[String, Int]](jTreeMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JNavigableMap[String, Int]](jTreeMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JTreeMap[String, Int]](jTreeMap, Map("1" -> 1, "2" -> 2, "3" -> 3))
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
    implicit val codec = GenCodec.auto[SomeObject.type]
  }

  test("object test") {
    testWriteRead(SomeObject, Map())
  }

  case class NoArgCaseClass()
  object NoArgCaseClass {
    implicit val codec = GenCodec.auto[NoArgCaseClass]
  }

  test("no arg case class test") {
    testWriteRead(NoArgCaseClass(), Map())
  }

  case class SingleArgCaseClass(str: String)
  object SingleArgCaseClass {
    implicit val codec = GenCodec.auto[SingleArgCaseClass]
  }

  test("single arg case class test") {
    testWriteRead(SingleArgCaseClass("something"), Map("str" -> "something"))
  }

  @transparent
  case class TransparentWrapper(str: String)
  object TransparentWrapper {
    implicit val codec = GenCodec.auto[TransparentWrapper]
  }

  test("transparent wrapper test") {
    testWriteRead(TransparentWrapper("something"), "something")
  }

  case class SomeCaseClass(@name("some.str") str: String, intList: List[Int])
  object SomeCaseClass {
    implicit val codec = GenCodec.auto[SomeCaseClass]
  }

  test("case class test") {
    testWriteRead(SomeCaseClass("dafuq", List(1, 2, 3)),
      Map("some.str" -> "dafuq", "intList" -> List(1, 2, 3))
    )
  }

  case class HasDefaults(@transientDefault int: Int = 42, str: String)
  object HasDefaults {
    implicit val codec = GenCodec.auto[HasDefaults]
  }

  test("case class with default values test") {
    testWriteRead(HasDefaults(str = "lol"), Map("str" -> "lol"))
    testWriteRead(HasDefaults(43, "lol"), Map("int" -> 43, "str" -> "lol"))
    testWriteRead(HasDefaults(str = null), Map("str" -> null))
  }

  case class Node[T](value: T, children: List[Node[T]] = Nil)
  object Node {
    implicit def codec[T: GenCodec]: GenCodec[Node[T]] = GenCodec.auto[Node[T]]
  }

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

  test("value class test") {
    testWriteRead(ValueClass("costam"), Map("str" -> "costam"))
  }

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  object Tree {
    implicit def codec[T: GenCodec]: GenCodec[Tree[T]] = GenCodec.auto[Tree[T]]
  }

  test("recursive gadt test") {
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

    implicit val codec: GenCodec[Enumz] = GenCodec.auto[Enumz]
  }

  test("sealed enum test") {
    testWriteRead[Enumz](Enumz.First, Map("Primary" -> Map()))
  }
}
