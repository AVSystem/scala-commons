package com.avsystem.commons
package jiop

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.Factory

final class ScalaJStream[+A](private val jStream: JStream[A @uV]) extends AnyVal {
  def asJava[B >: A]: JStream[B] =
    jStream.asInstanceOf[JStream[B]]

  def close(): Unit =
    jStream.close()

  def isParallel: Boolean =
    jStream.isParallel

  def parallel: ScalaJStream[A] =
    new ScalaJStream(jStream.parallel())

  inline def onClose(closeHandler: => Any): ScalaJStream[A] =
    new ScalaJStream(jStream.onClose(jRunnable(closeHandler)))

  def sequential: ScalaJStream[A] =
    new ScalaJStream(jStream.sequential())

  def unordered: ScalaJStream[A] =
    new ScalaJStream(jStream.unordered())

  def iterator: Iterator[A] =
    jStream.iterator().asScala

  def asDoubleStream(implicit ev: A <:< Double): ScalaJDoubleStream =
    mapToDouble(ev)

  def asIntStream(implicit ev: A <:< Int): ScalaJIntStream =
    mapToInt(ev)

  def asLongStream(implicit ev: A <:< Long): ScalaJLongStream =
    mapToLong(ev)

  inline def allMatch(inline predicate: A => Boolean): Boolean =
    jStream.allMatch(jPredicate(predicate))

  inline def anyMatch(inline predicate: A => Boolean): Boolean =
    jStream.anyMatch(jPredicate(predicate))

  def collect[R, B](collector: JCollector[_ >: A, B, R]): R =
    jStream.collect(collector)

  inline def collect[R](supplier: => R)(inline accumulator: (R, A) => Any, inline combiner: (R, R) => Any): R =
    jStream.collect(jSupplier(supplier), jBiConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    jStream.count()

  def distinct: ScalaJStream[A] =
    new ScalaJStream(jStream.distinct())

  inline def filter(inline predicate: A => Boolean): ScalaJStream[A] =
    new ScalaJStream(jStream.filter(jPredicate(predicate)))

  def findAny: Option[A] =
    jStream.findAny().asScala

  def findFirst: Option[A] =
    jStream.findFirst().asScala

  inline def flatMap[R](inline mapper: A => ScalaJStream[R]): ScalaJStream[R] =
    new ScalaJStream(jStream.flatMap(jFunction(t => mapper(t).jStream)))

  inline def flatMapToDouble(inline mapper: A => ScalaJDoubleStream): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.flatMapToDouble(jFunction(t => mapper(t).asJava)))

  inline def flatMapToInt(inline mapper: A => ScalaJIntStream): ScalaJIntStream =
    new ScalaJIntStream(jStream.flatMapToInt(jFunction(t => mapper(t).asJava)))

  inline def flatMapToLong(inline mapper: A => ScalaJLongStream): ScalaJLongStream =
    new ScalaJLongStream(jStream.flatMapToLong(jFunction(t => mapper(t).asJava)))

  inline def forEach(inline action: A => Any): Unit =
    jStream.forEach(jConsumer(action))

  inline def forEachOrdered(inline action: A => Any): Unit =
    jStream.forEachOrdered(jConsumer(action))

  def limit(maxSize: Long): ScalaJStream[A] =
    new ScalaJStream(jStream.limit(maxSize))

  inline def map[R](inline mapper: A => R): ScalaJStream[R] =
    new ScalaJStream(jStream.map[R](jFunction(mapper)))

  inline def mapToDouble(inline mapper: A => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.mapToDouble(jToDoubleFunction(mapper)))

  inline def mapToInt(inline mapper: A => Int): ScalaJIntStream =
    new ScalaJIntStream(jStream.mapToInt(jToIntFunction(mapper)))

  inline def mapToLong(inline mapper: A => Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.mapToLong(jToLongFunction(mapper)))

  inline def max(inline comparator: (A, A) => Int): Option[A] =
    jStream.max(jComparator(comparator)).asScala

  inline def min(inline comparator: (A, A) => Int): Option[A] =
    jStream.min(jComparator(comparator)).asScala

  inline def noneMatch(inline predicate: A => Boolean): Boolean =
    jStream.noneMatch(jPredicate(predicate))

  inline def peek(inline action: A => Any): ScalaJStream[A] =
    new ScalaJStream(jStream.peek(jConsumer(action)))

  inline def reduce[B >: A](inline accumulator: (B, B) => B): Option[B] =
    jStream.asInstanceOf[JStream[B]].reduce(jBinaryOperator(accumulator)).asScala

  inline def reduce[B >: A](identity: B)(inline accumulator: (B, B) => B): B =
    jStream.asInstanceOf[JStream[B]].reduce(identity, jBinaryOperator(accumulator))

  inline def reduce[U](identity: U)(inline accumulator: (U, A) => U, inline combiner: (U, U) => U): U =
    jStream.reduce(identity, jBiFunction(accumulator), jBinaryOperator(combiner))

  def skip(n: Long): ScalaJStream[A] =
    new ScalaJStream(jStream.skip(n))

  def sorted: ScalaJStream[A] =
    new ScalaJStream(jStream.sorted)

  inline def sorted(inline comparator: (A, A) => Int): ScalaJStream[A] =
    new ScalaJStream(jStream.sorted(jComparator(comparator)))

  def toArray[B >: A <: AnyRef: ClassTag]: Array[B] =
    jStream.toArray[B](jIntFunction(n => new Array[B](n)))

  def to[C](fac: Factory[A, C]): C = {
    val b = fac.newBuilder
    forEachOrdered(b += _)
    b.result()
  }
}
