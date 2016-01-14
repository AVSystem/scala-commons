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

  def testWriteRead[T : GenCodec](value: T): Unit = {
    var written: Any = null
    GenCodec.write[T](new SimpleValueOutput(written = _), value)
    println(written)
    val readBack = GenCodec.read[T](new SimpleValueInput(written))
    assertSameTypeValue(value, readBack)
  }

  test("NoState test") {
    type NoState = Nothing {type Dummy = Nothing}
    assert(implicitly[GenCodec[NoState]] == GenCodec.NothingCodec)
  }

  test("collection test") {
    testWriteRead[Option[Int]](option)
    testWriteRead[List[Int]](list)
    testWriteRead[Set[Int]](set)
    testWriteRead[Map[String, Int]](map)
    testWriteRead[Map[Int, Int]](intMap)
    testWriteRead[Map[Double, Int]](doubleMap)
    testWriteRead[IHashMap[String, Int]](hashMap)
  }

  test("java colleciton test") {
    testWriteRead[JCollection[Int]](jArrayList)
    testWriteRead[JList[Int]](jArrayList)
    testWriteRead[JArrayList[Int]](jArrayList)
    testWriteRead[JLinkedList[Int]](jLinkedList)
    testWriteRead[JSet[Int]](jHashSet)
    testWriteRead[JHashSet[Int]](jHashSet)
    testWriteRead[JLinkedHashSet[Int]](jLinkedHashSet)
    testWriteRead[JSortedSet[Int]](jTreeSet)
    testWriteRead[JNavigableSet[Int]](jTreeSet)
    testWriteRead[JTreeSet[Int]](jTreeSet)
    testWriteRead[JMap[String, Int]](jHashMap)
    testWriteRead[JHashMap[String, Int]](jHashMap)
    testWriteRead[JLinkedHashMap[String, Int]](jLinkedHashMap)
    testWriteRead[JHashMap[Int, Int]](jIntHashMap)
    testWriteRead[JHashMap[Double, Int]](jDoubleHashMap)
    testWriteRead[JSortedMap[String, Int]](jTreeMap)
    testWriteRead[JNavigableMap[String, Int]](jTreeMap)
    testWriteRead[JTreeMap[String, Int]](jTreeMap)
  }

  test("tuple test") {
    testWriteRead((1, 2, 3))
    testWriteRead((1, "lol"))
    testWriteRead((1, "lol", 3.0, 'a', List("dafuq", "fuu")))
  }

  object SomeObject {
    implicit val codec = GenCodec.auto[SomeObject.type]
  }

  test("object test") {
    testWriteRead(SomeObject)
  }

  case class NoArgCaseClass()
  object NoArgCaseClass {
    implicit val codec = GenCodec.auto[NoArgCaseClass]
  }

  test("no arg case class test") {
    testWriteRead(NoArgCaseClass())
  }

  case class SingleArgCaseClass(str: String)
  object SingleArgCaseClass {
    implicit val codec = GenCodec.auto[SingleArgCaseClass]
  }

  test("single arg case class test") {
    testWriteRead(SingleArgCaseClass("something"))
  }

  case class SomeCaseClass(@name("some.str") str: String, intList: List[Int])
  object SomeCaseClass {
    implicit val codec = GenCodec.auto[SomeCaseClass]
  }

  test("case class test") {
    testWriteRead(SomeCaseClass("dafuq", List(1, 2, 3)))
  }

  case class HasDefaults(@transientDefault int: Int = 42, str: String)
  object HasDefaults {
    implicit val codec = GenCodec.auto[HasDefaults]
  }

  test("case class with default values test") {
    testWriteRead(HasDefaults(str = "lol"))
    testWriteRead(HasDefaults(43, "lol"))
    testWriteRead(HasDefaults(str = null))
  }

  case class Node[T](value: T, children: List[Node[T]] = Nil)
  object Node {
    implicit def codec[T: GenCodec]: GenCodec[Node[T]] = GenCodec.auto[Node[T]]
  }

  test("recursive generic case class test") {
    testWriteRead(Node(123, List(Node(42, List(Node(52), Node(53))), Node(43))))
  }

  test("value class test") {
    testWriteRead(ValueClass("costam"))
  }

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  object Tree {
    implicit def codec[T: GenCodec]: GenCodec[Tree[T]] = GenCodec.auto[Tree[T]]
  }

  test("recursive gadt test") {
    testWriteRead[Tree[Int]](Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
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
    testWriteRead[Enumz](Enumz.Second)
  }
}
