package com.avsystem.commons
package jiop

import java.util.stream.{Collectors, DoubleStream, IntStream, LongStream}

import org.scalatest.FunSuite

class JavaInteropTest extends FunSuite {

  import JavaInterop._

  def assertSame[A](s1: JStream[A], s2: JStream[A]): Unit =
    assert(s1.collect(Collectors.toList[A]) === s2.collect(Collectors.toList[A]))

  test("adapted java stream api should work") {
    val input = JArrayList("a", "b", "c", "d", "e", "f", "g")
    assertSame(
      input.scalaStream.map(_.toUpperCase.charAt(0).toInt).filter(_ < 70).asJava,
      input.stream.map[Int](jFunction(_.toUpperCase.charAt(0).toInt)).filter(jPredicate(_ < 70))
    )
  }

  test("java primitive streams should work") {
    import JavaInterop._

    def ints = IntStream.of(1, 2, 3, 4, 5, 6)
    assertSame(
      ints.asScala.flatMap(i => JArrayList(i - 1, i, i + 1).scalaIntStream).asJava.boxed,
      ints.flatMap(jIntFunction(i => JArrayList(i - 1, i, i + 1)
        .stream.mapToInt(jToIntFunction(identity)))).boxed
    )
    def longs = LongStream.of(1, 2, 3, 4, 5, 6)
    assertSame(
      longs.asScala.flatMap(i => JArrayList(i - 1, i, i + 1).scalaLongStream).asJava.boxed,
      longs.flatMap(jLongFunction(i => JArrayList(i - 1, i, i + 1)
        .stream.mapToLong(jToLongFunction(identity)))).boxed
    )
    def doubles = DoubleStream.of(1, 2, 3, 4, 5, 6)
    assertSame(
      doubles.asScala.flatMap(i => JArrayList(i - 1, i, i + 1).scalaDoubleStream).asJava.boxed,
      doubles.flatMap(jDoubleFunction(i => JArrayList(i - 1, i, i + 1)
        .stream.mapToDouble(jToDoubleFunction(identity)))).boxed
    )
  }

  test("streams should be collectible to scala collections") {
    import JavaInterop._

    assert(JArrayList(1, 2, 3).scalaStream.to[Traversable] === Traversable(1, 2, 3))
    assert(JArrayList(1, 2, 3).scalaStream.to[Iterable] === Iterable(1, 2, 3))
    assert(JArrayList(1, 2, 3).scalaStream.to[Seq] === Seq(1, 2, 3))
    assert(JArrayList(1, 2, 3).scalaStream.to[List] === List(1, 2, 3))
    assert(JArrayList(1, 2, 3).scalaStream.to[Vector] === Vector(1, 2, 3))
    assert(JArrayList(1, 2, 3).scalaStream.to[Set] === Set(1, 2, 3))
  }

  test("streams should be collectible to java collections") {
    import JavaInterop._

    assert(JArrayList(1, 2, 3).scalaStream.to[JIterable] === JArrayList(1, 2, 3))
    assert(JArrayList(1, 2, 3).scalaStream.to[JArrayList] === JArrayList(1, 2, 3))
  }

  test("java collection CanBuildFroms should have proper priority") {
    import JavaInterop._

    assert(List(1, 2, 3).to[TraversableOnce] === List(1, 2, 3))
    assert(List(1, 2, 3).to[JArrayList] === JArrayList(1, 2, 3))
    assert(List(1, 2, 3).to[JLinkedList] === JLinkedList(1, 2, 3))
    assert(List(1, 2, 3).to[JList] === JArrayList(1, 2, 3))
    assert(List(1, 2, 3).to[JLinkedHashSet] === JLinkedHashSet(1, 2, 3))
    assert(List(1, 2, 3).to[JHashSet] === JHashSet(1, 2, 3))
    assert(List(1, 2, 3).to[JTreeSet] === JTreeSet(1, 2, 3))
    assert(List(1, 2, 3).to[JNavigableSet] === JTreeSet(1, 2, 3))
    assert(List(1, 2, 3).to[JSortedSet] === JTreeSet(1, 2, 3))
    assert(List(1, 2, 3).to[JSet] === JHashSet(1, 2, 3))
    assert(List(1, 2, 3).to[JCollection] === JArrayList(1, 2, 3))
    assert(List(1, 2, 3).to[JIterable] === JArrayList(1, 2, 3))
  }
}
