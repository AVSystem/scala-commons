package com.avsystem.commons
package jiop

import java.util.stream.{Collectors, DoubleStream, IntStream, LongStream}

import com.google.common.util.concurrent.{MoreExecutors, SettableFuture}
import org.scalatest.FunSuite

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Promise}
import scala.util.Success

class JavaInteropTest extends FunSuite {

  import JavaInterop._

  def assertSame[A](s1: JStream[A], s2: JStream[A]): Unit =
    assert(s1.collect(Collectors.toList[A]) === s2.collect(Collectors.toList[A]))

  def assertSameTypeValue(v1: Any, v2: Any): Unit = {
    assert(v1 === v2)
    assert(v1.getClass === v2.getClass)
  }

  def col[T <: JCollection[Int]](col: T): T = {
    col.add(1)
    col.add(2)
    col.add(3)
    col
  }

  def map[M <: JMap[Int, String]](map: M): M = {
    map.put(1, "1")
    map.put(2, "2")
    map.put(3, "3")
    map
  }

  val arrayList = col(new JArrayList[Int])
  val linkedList = col(new JLinkedList[Int])
  val hashSet = col(new JHashSet[Int])
  val linkedHashSet = col(new JLinkedHashSet[Int])
  val treeSet = col(new JTreeSet[Int])
  val hashMap = map(new JHashMap[Int, String])
  val linkedHashMap = map(new JLinkedHashMap[Int, String])
  val treeMap = map(new JTreeMap[Int, String])

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

    assertSameTypeValue(arrayList.scalaStream.to[Traversable], Iterator(1, 2, 3).to[Traversable])
    assertSameTypeValue(arrayList.scalaStream.to[Iterable], Iterator(1, 2, 3).to[Iterable])
    assertSameTypeValue(arrayList.scalaStream.to[Seq], Iterator(1, 2, 3).to[Seq])
    assertSameTypeValue(arrayList.scalaStream.to[List], Iterator(1, 2, 3).to[List])
    assertSameTypeValue(arrayList.scalaStream.to[Vector], Iterator(1, 2, 3).to[Vector])
    assertSameTypeValue(arrayList.scalaStream.to[Set], Iterator(1, 2, 3).to[Set])
  }

  test("streams should be collectible to java collections") {
    assertSameTypeValue(arrayList.scalaStream.to[JIterable], arrayList)
    assertSameTypeValue(arrayList.scalaStream.to[JArrayList], arrayList)
  }

  test("java collection CanBuildFroms should have proper priority") {
    import JavaInterop._

    val intList = List(1, 2, 3)
    val pairList = intList.map(i => (i, i.toString))
    assertSameTypeValue(intList.to[JArrayList], arrayList)
    assertSameTypeValue(intList.to[JLinkedList], linkedList)
    assertSameTypeValue(intList.to[JList], arrayList)
    assertSameTypeValue(intList.to[JLinkedHashSet], linkedHashSet)
    assertSameTypeValue(intList.to[JHashSet], hashSet)
    assertSameTypeValue(intList.to[JTreeSet], treeSet)
    assertSameTypeValue(intList.to[JNavigableSet], treeSet)
    assertSameTypeValue(intList.to[JSortedSet], treeSet)
    assertSameTypeValue(intList.to[JSet], hashSet)
    assertSameTypeValue(intList.to[JCollection], arrayList)
    assertSameTypeValue(intList.to[JIterable], arrayList)
    assertSameTypeValue(pairList.toJMap, hashMap)
    assertSameTypeValue(pairList.toJMap[JMap], hashMap)
    assertSameTypeValue(pairList.toJMap[JHashMap], hashMap)
    assertSameTypeValue(pairList.toJMap[JLinkedHashMap], linkedHashMap)
    assertSameTypeValue(pairList.toJMap[JSortedMap], treeMap)
    assertSameTypeValue(pairList.toJMap[JNavigableMap], treeMap)
    assertSameTypeValue(pairList.toJMap[JTreeMap], treeMap)
  }

  test("java collection creators should work") {
    assertSameTypeValue(JIterable(1, 2, 3), arrayList)
    assertSameTypeValue(JCollection(1, 2, 3), arrayList)
    assertSameTypeValue(JList(1, 2, 3), arrayList)
    assertSameTypeValue(JArrayList(1, 2, 3), arrayList)
    assertSameTypeValue(JLinkedList(1, 2, 3), linkedList)
    assertSameTypeValue(JSet(1, 2, 3), hashSet)
    assertSameTypeValue(JHashSet(1, 2, 3), hashSet)
    assertSameTypeValue(JLinkedHashSet(1, 2, 3), linkedHashSet)
    assertSameTypeValue(JSortedSet(1, 2, 3), treeSet)
    assertSameTypeValue(JNavigableSet(1, 2, 3), treeSet)
    assertSameTypeValue(JTreeSet(1, 2, 3), treeSet)
    assertSameTypeValue(JMap(1 -> "1", 2 -> "2", 3 -> "3"), hashMap)
    assertSameTypeValue(JHashMap(1 -> "1", 2 -> "2", 3 -> "3"), hashMap)
    assertSameTypeValue(JLinkedHashMap(1 -> "1", 2 -> "2", 3 -> "3"), linkedHashMap)
    assertSameTypeValue(JSortedMap(1 -> "1", 2 -> "2", 3 -> "3"), treeMap)
    assertSameTypeValue(JNavigableMap(1 -> "1", 2 -> "2", 3 -> "3"), treeMap)
    assertSameTypeValue(JTreeMap(1 -> "1", 2 -> "2", 3 -> "3"), treeMap)
  }

  test("java collection extractors should work") {
    arrayList match {
      case JList(1, 2, 3) =>
    }
    arrayList match {
      case JArrayList(1, 2, 3) =>
    }
    linkedList match {
      case JList(1, 2, 3) =>
    }
    linkedList match {
      case JLinkedList(1, 2, 3) =>
    }
    linkedHashSet match {
      case JLinkedHashSet(1, 2, 3) =>
    }
    treeSet match {
      case JSortedSet(1, 2, 3) =>
    }
    treeSet match {
      case JNavigableSet(1, 2, 3) =>
    }
    treeSet match {
      case JTreeSet(1, 2, 3) =>
    }
    linkedHashMap match {
      case JLinkedHashMap((1, "1"), (2, "2"), (3, "3")) =>
    }
    treeMap match {
      case JSortedMap((1, "1"), (2, "2"), (3, "3")) =>
    }
    treeMap match {
      case JNavigableMap((1, "1"), (2, "2"), (3, "3")) =>
    }
    treeMap match {
      case JTreeMap((1, "1"), (2, "2"), (3, "3")) =>
    }
  }

  test("scala to guava Future conversion should work") {
    val promise = Promise[Int]()
    val sfut = promise.future
    val gfut = sfut.asGuava
    var listenerCalled: Boolean = false

    assert(gfut.isDone === sfut.isCompleted)

    gfut.addListener(jRunnable(listenerCalled = true), MoreExecutors.sameThreadExecutor())
    promise.success(123)

    assert(Await.result(sfut, Duration.Inf) === gfut.get)
    assert(gfut.isDone === sfut.isCompleted)
    assert(listenerCalled)
  }

  test("guava to scala Future conversion should work") {
    implicit object ec extends ExecutionContext {
      def execute(runnable: Runnable): Unit = runnable.run()
      def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
    }

    val gfut = SettableFuture.create[Int]
    val sfut = gfut.asScala
    var listenerCalled: Boolean = false

    assert(gfut.isDone === sfut.isCompleted)
    assert(None === sfut.value)

    sfut.onComplete(_ => listenerCalled = true)
    gfut.set(123)

    assert(Await.result(sfut, Duration.Inf) === gfut.get)
    assert(gfut.isDone === sfut.isCompleted)
    assert(sfut.value === Some(Success(123)))
    assert(listenerCalled)
  }

  test("toJMap should work") {
    val jmap = Iterator(1 -> "jeden", 2 -> "dwa").toJMap
    assert(jmap === JMap(1 -> "jeden", 2 -> "dwa"))
  }
}
