package com.avsystem.commons
package jiop

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

final class ScalaJStream[+A](private val jStream: JStream[A@uV]) extends AnyVal {
  def asJava[B >: A]: JStream[B] =
    jStream.asInstanceOf[JStream[B]]

  def close(): Unit =
    jStream.close()

  def isParallel: Boolean =
    jStream.isParallel

  def parallel: ScalaJStream[A] =
    new ScalaJStream(jStream.parallel())

  def onClose(closeHandler: => Any): ScalaJStream[A] =
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

  def allMatch(predicate: A => Boolean): Boolean =
    jStream.allMatch(jPredicate(predicate))

  def anyMatch(predicate: A => Boolean): Boolean =
    jStream.anyMatch(jPredicate(predicate))

  def collect[R, B](collector: JCollector[_ >: A, B, R]): R =
    jStream.collect(collector)

  def collect[R](supplier: => R)(accumulator: (R, A) => Any, combiner: (R, R) => Any): R =
    jStream.collect(jSupplier(supplier), jBiConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    jStream.count()

  def distinct: ScalaJStream[A] =
    new ScalaJStream(jStream.distinct())

  def filter(predicate: A => Boolean): ScalaJStream[A] =
    new ScalaJStream(jStream.filter(jPredicate(predicate)))

  def findAny: Option[A] =
    jStream.findAny().asScala

  def findFirst: Option[A] =
    jStream.findFirst().asScala

  def flatMap[R](mapper: A => ScalaJStream[R]): ScalaJStream[R] =
    new ScalaJStream(jStream.flatMap(jFunction(t => mapper(t).jStream)))

  def flatMapToDouble(mapper: A => ScalaJDoubleStream): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.flatMapToDouble(jFunction(t => mapper(t).asJava)))

  def flatMapToInt(mapper: A => ScalaJIntStream): ScalaJIntStream =
    new ScalaJIntStream(jStream.flatMapToInt(jFunction(t => mapper(t).asJava)))

  def flatMapToLong(mapper: A => ScalaJLongStream): ScalaJLongStream =
    new ScalaJLongStream(jStream.flatMapToLong(jFunction(t => mapper(t).asJava)))

  def forEach(action: A => Any): Unit =
    jStream.forEach(jConsumer(action))

  def forEachOrdered(action: A => Any): Unit =
    jStream.forEachOrdered(jConsumer(action))

  def limit(maxSize: Long): ScalaJStream[A] =
    new ScalaJStream(jStream.limit(maxSize))

  def map[R](mapper: A => R): ScalaJStream[R] =
    new ScalaJStream(jStream.map[R](jFunction(mapper)))

  def mapToDouble(mapper: A => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.mapToDouble(jToDoubleFunction(mapper)))

  def mapToInt(mapper: A => Int): ScalaJIntStream =
    new ScalaJIntStream(jStream.mapToInt(jToIntFunction(mapper)))

  def mapToLong(mapper: A => Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.mapToLong(jToLongFunction(mapper)))

  def max(comparator: (A, A) => Int): Option[A] =
    jStream.max(jComparator(comparator)).asScala

  def min(comparator: (A, A) => Int): Option[A] =
    jStream.min(jComparator(comparator)).asScala

  def noneMatch(predicate: A => Boolean): Boolean =
    jStream.noneMatch(jPredicate(predicate))

  def peek(action: A => Any): ScalaJStream[A] =
    new ScalaJStream(jStream.peek(jConsumer(action)))

  def reduce[B >: A](accumulator: (B, B) => B): Option[B] =
    jStream.asInstanceOf[JStream[B]].reduce(jBinaryOperator(accumulator)).asScala

  def reduce[B >: A](identity: B)(accumulator: (B, B) => B): B =
    jStream.asInstanceOf[JStream[B]].reduce(identity, jBinaryOperator(accumulator))

  def reduce[U](identity: U)(accumulator: (U, A) => U, combiner: (U, U) => U): U =
    jStream.reduce(identity, jBiFunction(accumulator), jBinaryOperator(combiner))

  def skip(n: Long): ScalaJStream[A] =
    new ScalaJStream(jStream.skip(n))

  def sorted: ScalaJStream[A] =
    new ScalaJStream(jStream.sorted)

  def sorted(comparator: (A, A) => Int): ScalaJStream[A] =
    new ScalaJStream(jStream.sorted(jComparator(comparator)))

  def toArray[B >: A <: AnyRef : ClassTag]: Array[B] =
    jStream.toArray[B](jIntFunction(n => new Array[B](n)))

  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A@uV]]): Col[A@uV] = {
    val b = cbf.apply()
    forEachOrdered(b += _)
    b.result()
  }
}
