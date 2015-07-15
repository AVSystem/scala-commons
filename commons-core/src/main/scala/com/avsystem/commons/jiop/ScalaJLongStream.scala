package com.avsystem.commons
package jiop

import java.util.{LongSummaryStatistics, Spliterator}

import com.avsystem.commons.jiop.JavaInterop._

/**
 * Author: ghik
 * Created: 15/07/15.
 */
final class ScalaJLongStream(val asJava: JLongStream) extends AnyVal {
  def close(): Unit =
    asJava.close()

  def spliterator: Spliterator[Long] =
    asJava.spliterator().asInstanceOf[Spliterator[Long]]

  def isParallel: Boolean =
    asJava.isParallel

  def iterator: Iterator[Long] =
    asJava.iterator().asInstanceOf[JIterator[Long]].asScala

  def onClose(closeHandler: => Any): ScalaJLongStream =
    new ScalaJLongStream(asJava.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJLongStream =
    new ScalaJLongStream(asJava.parallel())

  def sequential: ScalaJLongStream =
    new ScalaJLongStream(asJava.sequential())

  def unordered: ScalaJLongStream =
    new ScalaJLongStream(asJava.unordered())

  def allMatch(predicate: Long => Boolean): Boolean =
    asJava.allMatch(jLongPredicate(predicate))

  def anyMatch(predicate: Long => Boolean): Boolean =
    asJava.allMatch(jLongPredicate(predicate))

  def asDoubleStream: ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.asDoubleStream())

  def average: Option[Double] =
    asJava.average.asScala

  def boxed: ScalaJStream[Long] =
    new ScalaJStream(asJava.boxed.asInstanceOf[JStream[Long]])

  def collect[R](supplier: => R)(accumulator: (R, Long) => Any, combiner: (R, R) => Any): R =
    asJava.collect(jSupplier(supplier), jObjLongConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    asJava.count

  def distinct: ScalaJLongStream =
    new ScalaJLongStream(asJava.distinct)

  def filter(predicate: Long => Boolean): ScalaJLongStream =
    new ScalaJLongStream(asJava.filter(jLongPredicate(predicate)))

  def findAny: Option[Long] =
    asJava.findAny().asScala

  def findFirst: Option[Long] =
    asJava.findFirst.asScala

  def flatMap(mapper: Long => ScalaJLongStream): ScalaJLongStream =
    new ScalaJLongStream(asJava.flatMap(jLongFunction(d => mapper(d).asJava)))

  def forEach(action: Long => Any): Unit =
    asJava.forEach(jLongConsumer(action))

  def forEachOrdered(action: Long => Any): Unit =
    asJava.forEachOrdered(jLongConsumer(action))

  def limit(maxSize: Long): ScalaJLongStream =
    new ScalaJLongStream(asJava.limit(maxSize))

  def map(mapper: Long => Long): ScalaJLongStream =
    new ScalaJLongStream(asJava.map(jLongUnaryOperator(mapper)))

  def mapToDouble(mapper: Long => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.mapToDouble(jLongToDoubleFunction(mapper)))

  def mapToInt(mapper: Long => Int): ScalaJIntStream =
    new ScalaJIntStream(asJava.mapToInt(jLongToIntFunction(mapper)))

  def mapToObj[U](mapper: Long => U): ScalaJStream[U] =
    new ScalaJStream(asJava.mapToObj(jLongFunction(mapper)))

  def max: Option[Long] =
    asJava.max.asScala

  def min: Option[Long] =
    asJava.min.asScala

  def noneMatch(predicate: Long => Boolean): Boolean =
    asJava.noneMatch(jLongPredicate(predicate))

  def peek(action: Long => Any): ScalaJLongStream =
    new ScalaJLongStream(asJava.peek(jLongConsumer(action)))

  def reduce(identity: Long)(op: (Long, Long) => Long): Long =
    asJava.reduce(identity, jLongBinaryOperator(op))

  def reduce(op: (Long, Long) => Long): Option[Long] =
    asJava.reduce(jLongBinaryOperator(op)).asScala

  def skip(n: Long): ScalaJLongStream =
    new ScalaJLongStream(asJava.skip(n))

  def sorted: ScalaJLongStream =
    new ScalaJLongStream(asJava.sorted)

  def sum: Long =
    asJava.sum

  def summaryStatistics: LongSummaryStatistics =
    asJava.summaryStatistics()

  def toArray: Array[Long] =
    asJava.toArray
}
