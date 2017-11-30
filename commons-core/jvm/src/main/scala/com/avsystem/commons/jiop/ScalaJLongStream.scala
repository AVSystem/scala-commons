package com.avsystem.commons
package jiop

import java.util.LongSummaryStatistics

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

final class ScalaJLongStream(private val jStream: JLongStream) extends AnyVal {
  def asJava = jStream

  def close(): Unit =
    jStream.close()

  def isParallel: Boolean =
    jStream.isParallel

  def iterator: Iterator[Long] =
    jStream.iterator().asInstanceOf[JIterator[Long]].asScala

  def onClose(closeHandler: => Any): ScalaJLongStream =
    new ScalaJLongStream(jStream.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJLongStream =
    new ScalaJLongStream(jStream.parallel())

  def sequential: ScalaJLongStream =
    new ScalaJLongStream(jStream.sequential())

  def unordered: ScalaJLongStream =
    new ScalaJLongStream(jStream.unordered())

  def allMatch(predicate: Long => Boolean): Boolean =
    jStream.allMatch(jLongPredicate(predicate))

  def anyMatch(predicate: Long => Boolean): Boolean =
    jStream.allMatch(jLongPredicate(predicate))

  def asDoubleStream: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.asDoubleStream())

  def average: Option[Double] =
    jStream.average.asScala

  def boxed: ScalaJStream[Long] =
    new ScalaJStream(jStream.boxed.asInstanceOf[JStream[Long]])

  def collect[R](supplier: => R)(accumulator: (R, Long) => Any, combiner: (R, R) => Any): R =
    jStream.collect(jSupplier(supplier), jObjLongConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    jStream.count

  def distinct: ScalaJLongStream =
    new ScalaJLongStream(jStream.distinct)

  def filter(predicate: Long => Boolean): ScalaJLongStream =
    new ScalaJLongStream(jStream.filter(jLongPredicate(predicate)))

  def findAny: Option[Long] =
    jStream.findAny().asScala

  def findFirst: Option[Long] =
    jStream.findFirst.asScala

  def flatMap(mapper: Long => ScalaJLongStream): ScalaJLongStream =
    new ScalaJLongStream(jStream.flatMap(jLongFunction(d => mapper(d).jStream)))

  def forEach(action: Long => Any): Unit =
    jStream.forEach(jLongConsumer(action))

  def forEachOrdered(action: Long => Any): Unit =
    jStream.forEachOrdered(jLongConsumer(action))

  def limit(maxSize: Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.limit(maxSize))

  def map(mapper: Long => Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.map(jLongUnaryOperator(mapper)))

  def mapToDouble(mapper: Long => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.mapToDouble(jLongToDoubleFunction(mapper)))

  def mapToInt(mapper: Long => Int): ScalaJIntStream =
    new ScalaJIntStream(jStream.mapToInt(jLongToIntFunction(mapper)))

  def mapToObj[U](mapper: Long => U): ScalaJStream[U] =
    new ScalaJStream(jStream.mapToObj(jLongFunction(mapper)))

  def max: Option[Long] =
    jStream.max.asScala

  def min: Option[Long] =
    jStream.min.asScala

  def noneMatch(predicate: Long => Boolean): Boolean =
    jStream.noneMatch(jLongPredicate(predicate))

  def peek(action: Long => Any): ScalaJLongStream =
    new ScalaJLongStream(jStream.peek(jLongConsumer(action)))

  def reduce(identity: Long)(op: (Long, Long) => Long): Long =
    jStream.reduce(identity, jLongBinaryOperator(op))

  def reduce(op: (Long, Long) => Long): Option[Long] =
    jStream.reduce(jLongBinaryOperator(op)).asScala

  def skip(n: Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.skip(n))

  def sorted: ScalaJLongStream =
    new ScalaJLongStream(jStream.sorted)

  def sum: Long =
    jStream.sum

  def summaryStatistics: LongSummaryStatistics =
    jStream.summaryStatistics()

  def toArray: Array[Long] =
    jStream.toArray

  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, Long, Col[Long]]): Col[Long] = {
    val b = cbf.apply()
    forEachOrdered(b += _)
    b.result()
  }

}
