package com.avsystem.commons
package concurrent

import monix.reactive.Observable
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.duration._

class ObservableBlockingIteratorTest extends AnyFunSuite {
  test("empty") {
    assert(DefaultBlocking.toIterator(Observable.empty).toList == Nil)
  }

  test("failing") {
    class BadStuff extends Exception
    val it = DefaultBlocking.toIterator(Observable.raiseError(new BadStuff))
    intercept[BadStuff](it.hasNext)
    intercept[BadStuff](it.next())
  }

  test("single element") {
    assert(DefaultBlocking.toIterator(Observable(1)).toList == List(1))
  }

  test("immediate observable") {
    val input = List(1, 2, 3)
    assert(DefaultBlocking.toIterator(Observable.fromIterable(input)).toList == input)
  }

  test("immediate, large observable") {
    val input = List.range(0, 1000)
    assert(DefaultBlocking.toIterator(Observable.fromIterable(input)).toList == input)
  }

  def slowlyToList[T](it: Iterator[T]): List[T] = {
    val lb = new MListBuffer[T]
    Thread.sleep(2)
    while (it.hasNext) {
      Thread.sleep(2)
      lb += it.next()
    }
    lb.result()
  }

  test("slow consumer") {
    val input = List.range(0, 1000)
    assert(slowlyToList(DefaultBlocking.toIterator(Observable.fromIterable(input))) == input)
  }

  test("slow producer") {
    val input = List.range(0, 1000)
    val observable = Observable.fromIterable(input).flatMap(i => Observable.evalDelayed(2.millis, i))
    assert(DefaultBlocking.toIterator(observable).toList == input)
  }

  test("slow producer and consumer") {
    val input = List.range(0, 1000)
    val observable = Observable.fromIterable(input).flatMap(i => Observable.evalDelayed(2.millis, i))
    assert(slowlyToList(DefaultBlocking.toIterator(observable)) == input)
  }
}
