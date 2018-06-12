package com.avsystem.commons
package serialization

import com.github.ghik.silencer.silent
import org.scalactic.source.Position
import org.scalatest.FunSuite

trait CodecTestBase extends FunSuite {
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

  val jArrayList = col(new JArrayList[Int])
  val jLinkedList = col(new JLinkedList[Int])
  val jHashSet = col(new JHashSet[Int])
  val jLinkedHashSet = col(new JLinkedHashSet[Int])
  val jTreeSet = col(new JTreeSet[Int])
  val jHashMap = stringMap(new JHashMap[String, Int])
  val jIntHashMap = map(new JHashMap[Int, Int])
  val jDoubleHashMap = doubleMap(new JHashMap[Double, Int])
  val jLinkedHashMap = stringMap(new JLinkedHashMap[String, Int])

  val option = Option(42)
  val list = List(1, 2, 3)
  val set = Set(1, 2, 3)
  val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
  val hashMap = IHashMap("1" -> 1, "2" -> 2, "3" -> 3)
  val intMap = Map(1 -> 1, 2 -> 2, 3 -> 3)
  val doubleMap = Map(1.0 -> 1, 2.0 -> 2, 3.0 -> 3)

  def assertSameTypeValue[T](v1: T, v2: T)(implicit pos: Position): Unit = {
    assert(v1 == v2)
    assert(v1.getClass == v2.getClass)
  }

  def testWriteRead[T: GenCodec](value: T, expectedRepr: Any)(implicit pos: Position): Unit = {
    testWriteReadWithVerify[T, Any](value, w => assert(w == expectedRepr))
  }

  def testWriteReadWithVerify[T: GenCodec, E](value: T, verifyWritten: E => Unit)(implicit pos: Position): Unit = {
    var written: Any = null
    GenCodec.write[T](new SimpleValueOutput(written = _), value)
    assert(written.isInstanceOf[E]): @silent
    verifyWritten(written.asInstanceOf[E])
    val readBack = GenCodec.read[T](new SimpleValueInput(written))
    assertSameTypeValue(value, readBack)
  }

  @silent
  def testAutoWriteRead[T: GenCodec.Auto](value: T, expectedRepr: Any)(implicit pos: Position): Unit = {
    testAutoWriteReadWithVerify[T, Any](value, w => assert(w == expectedRepr))
  }

  @silent
  def testAutoWriteReadWithVerify[T: GenCodec.Auto, E](value: T, verifyWritten: E => Unit)(implicit pos: Position): Unit = {
    var written: Any = null
    GenCodec.autoWrite[T](new SimpleValueOutput(written = _), value)
    assert(written.isInstanceOf[E]): @silent
    verifyWritten(written.asInstanceOf[E])
    val readBack = GenCodec.autoRead[T](new SimpleValueInput(written))
    assertSameTypeValue(value, readBack)
  }

  @silent
  def testWriteReadAndAutoWriteRead[T: GenCodec : GenCodec.Auto](value: T, expectedRepr: Any)(implicit pos: Position): Unit = {
    testWriteReadWithVerify[T, Any](value, w => assert(w == expectedRepr))
    testAutoWriteReadWithVerify[T, Any](value, w => assert(w == expectedRepr))
  }

  @silent
  def testSameElementsWriteRead[T: GenCodec : GenCodec.Auto](value: T, expectedRepr: Iterable[Any])(implicit pos: Position): Unit = {
    testWriteReadWithVerify[T, Iterable[Any]](value, w => assert(w.toSet == expectedRepr.toSet))
    testAutoWriteReadWithVerify[T, Iterable[Any]](value, w => assert(w.toSet == expectedRepr.toSet))
  }

  @silent
  def testReadAndAutoRead[T: GenCodec : GenCodec.Auto](repr: Any, expected: T)(implicit pos: Position): Unit = {
    assert(expected == GenCodec.read[T](new SimpleValueInput(repr)))
    assert(expected == GenCodec.autoRead[T](new SimpleValueInput(repr))): @silent
  }

}
