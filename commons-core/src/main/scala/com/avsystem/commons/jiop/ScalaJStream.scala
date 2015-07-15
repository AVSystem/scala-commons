package com.avsystem.commons
package jiop

import java.util.Spliterator

import com.avsystem.commons.jiop.JavaInterop._

import scala.reflect.ClassTag

/**
 * Author: ghik
 * Created: 15/07/15.
 */
final class ScalaJStream[T](val asJava: JStream[T]) extends AnyVal {
  def close(): Unit =
    asJava.close()

  def spliterator: Spliterator[T] =
    asJava.spliterator()

  def isParallel: Boolean =
    asJava.isParallel

  def parallel: ScalaJStream[T] =
    new ScalaJStream(asJava.parallel())

  def onClose(closeHandler: => Any): ScalaJStream[T] =
    new ScalaJStream(asJava.onClose(jRunnable(closeHandler)))

  def sequential: ScalaJStream[T] =
    new ScalaJStream(asJava.sequential())

  def unordered: ScalaJStream[T] =
    new ScalaJStream(asJava.unordered())

  def iterator: Iterator[T] =
    asJava.iterator().asScala

  def asDoubleStream(implicit ev: T <:< Double): ScalaJDoubleStream =
    mapToDouble(ev)

  def asIntStream(implicit ev: T <:< Int): ScalaJIntStream =
    mapToInt(ev)

  def asLongStream(implicit ev: T <:< Long): ScalaJLongStream =
    mapToLong(ev)

  def allMatch(predicate: T => Boolean): Boolean =
    asJava.allMatch(jPredicate(predicate))

  def anyMatch(predicate: T => Boolean): Boolean =
    asJava.anyMatch(jPredicate(predicate))

  def collect[R, A](collector: JCollector[_ >: T, A, R]): R =
    asJava.collect(collector)

  def collect[R](supplier: => R)(accumulator: (R, T) => Any, combiner: (R, R) => Any): R =
    asJava.collect(jSupplier(supplier), jBiConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    asJava.count()

  def distinct: ScalaJStream[T] =
    new ScalaJStream(asJava.distinct())

  def filter(predicate: T => Boolean): ScalaJStream[T] =
    new ScalaJStream(asJava.filter(jPredicate(predicate)))

  def findAny: Option[T] =
    asJava.findAny().asScala

  def findFirst: Option[T] =
    asJava.findFirst().asScala

  def flatMap[R](mapper: T => ScalaJStream[R]): ScalaJStream[R] =
    new ScalaJStream(asJava.flatMap(jFunction(t => mapper(t).asJava)))

  def flatMapToDouble(mapper: T => ScalaJDoubleStream): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.flatMapToDouble(jFunction(t => mapper(t).asJava)))

  def flatMapToInt(mapper: T => ScalaJIntStream): ScalaJIntStream =
    new ScalaJIntStream(asJava.flatMapToInt(jFunction(t => mapper(t).asJava)))

  def flatMapToLong(mapper: T => ScalaJLongStream): ScalaJLongStream =
    new ScalaJLongStream(asJava.flatMapToLong(jFunction(t => mapper(t).asJava)))

  def forEach(action: T => Any): Unit =
    asJava.forEach(jConsumer(action))

  def forEachOrdered(action: T => Any): Unit =
    asJava.forEachOrdered(jConsumer(action))

  def limit(maxSize: Long): ScalaJStream[T] =
    new ScalaJStream(asJava.limit(maxSize))

  def map[R](mapper: T => R): ScalaJStream[R] =
    new ScalaJStream(asJava.map(jFunction(mapper)))

  def mapToDouble(mapper: T => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.mapToDouble(jToDoubleFunction(mapper)))

  def mapToInt(mapper: T => Int): ScalaJIntStream =
    new ScalaJIntStream(asJava.mapToInt(jToIntFunction(mapper)))

  def mapToLong(mapper: T => Long): ScalaJLongStream =
    new ScalaJLongStream(asJava.mapToLong(jToLongFunction(mapper)))

  def max(comparator: (T, T) => Int): Option[T] =
    asJava.max(jComparator(comparator)).asScala

  def min(comparator: (T, T) => Int): Option[T] =
    asJava.min(jComparator(comparator)).asScala

  def noneMatch(predicate: T => Boolean): Boolean =
    asJava.noneMatch(jPredicate(predicate))

  def peek(action: T => Any): ScalaJStream[T] =
    new ScalaJStream(asJava.peek(jConsumer(action)))

  def reduce(accumulator: (T, T) => T): Option[T] =
    asJava.reduce(jBinaryOperator(accumulator)).asScala

  def reduce(identity: T)(accumulator: (T, T) => T): T =
    asJava.reduce(identity, jBinaryOperator(accumulator))

  def reduce[U](identity: U)(accumulator: (U, T) => U, combiner: (U, U) => U): U =
    asJava.reduce(identity, jBiFunction(accumulator), jBinaryOperator(combiner))

  def skip(n: Long): ScalaJStream[T] =
    new ScalaJStream(asJava.skip(n))

  def sorted: ScalaJStream[T] =
    new ScalaJStream(asJava.sorted)

  def sorted(comparator: (T, T) => Int): ScalaJStream[T] =
    new ScalaJStream(asJava.sorted(jComparator(comparator)))

  def toArray[A >: T <: AnyRef : ClassTag]: Array[A] =
    asJava.toArray[A](jIntFunction(n => new Array[A](n)))
}
