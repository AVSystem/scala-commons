package com.avsystem.commons
package jiop

import java.util.LongSummaryStatistics
import scala.collection.Factory

final class ScalaJLongStream(private val jStream: JLongStream) extends AnyVal {
  def asJava: JLongStream = jStream

  def close(): Unit =
    jStream.close()

  def isParallel: Boolean =
    jStream.isParallel

  def iterator: Iterator[Long] =
    jStream.iterator().asInstanceOf[JIterator[Long]].asScala

  inline def onClose(closeHandler: => Any): ScalaJLongStream =
    new ScalaJLongStream(jStream.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJLongStream =
    new ScalaJLongStream(jStream.parallel())

  def sequential: ScalaJLongStream =
    new ScalaJLongStream(jStream.sequential())

  def unordered: ScalaJLongStream =
    new ScalaJLongStream(jStream.unordered())

  inline def allMatch(inline predicate: Long => Boolean): Boolean =
    jStream.allMatch(jLongPredicate(predicate))

  inline def anyMatch(inline predicate: Long => Boolean): Boolean =
    jStream.anyMatch(jLongPredicate(predicate))

  def asDoubleStream: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.asDoubleStream())

  def average: Option[Double] =
    jStream.average.asScala

  def boxed: ScalaJStream[Long] =
    new ScalaJStream(jStream.boxed.asInstanceOf[JStream[Long]])

  inline def collect[R](supplier: => R)(inline accumulator: (R, Long) => Any, inline combiner: (R, R) => Any): R =
    jStream.collect(jSupplier(supplier), jObjLongConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    jStream.count

  def distinct: ScalaJLongStream =
    new ScalaJLongStream(jStream.distinct)

  inline def filter(inline predicate: Long => Boolean): ScalaJLongStream =
    new ScalaJLongStream(jStream.filter(jLongPredicate(predicate)))

  def findAny: Option[Long] =
    jStream.findAny().asScala

  def findFirst: Option[Long] =
    jStream.findFirst.asScala

  inline def flatMap(inline mapper: Long => ScalaJLongStream): ScalaJLongStream =
    new ScalaJLongStream(jStream.flatMap(jLongFunction(d => mapper(d).jStream)))

  inline def forEach(inline action: Long => Any): Unit =
    jStream.forEach(jLongConsumer(action))

  inline def forEachOrdered(inline action: Long => Any): Unit =
    jStream.forEachOrdered(jLongConsumer(action))

  def limit(maxSize: Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.limit(maxSize))

  inline def map(inline mapper: Long => Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.map(jLongUnaryOperator(mapper)))

  inline def mapToDouble(inline mapper: Long => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.mapToDouble(jLongToDoubleFunction(mapper)))

  inline def mapToInt(inline mapper: Long => Int): ScalaJIntStream =
    new ScalaJIntStream(jStream.mapToInt(jLongToIntFunction(mapper)))

  inline def mapToObj[U](inline mapper: Long => U): ScalaJStream[U] =
    new ScalaJStream(jStream.mapToObj(jLongFunction(mapper)))

  def max: Option[Long] =
    jStream.max.asScala

  def min: Option[Long] =
    jStream.min.asScala

  inline def noneMatch(inline predicate: Long => Boolean): Boolean =
    jStream.noneMatch(jLongPredicate(predicate))

  inline def peek(inline action: Long => Any): ScalaJLongStream =
    new ScalaJLongStream(jStream.peek(jLongConsumer(action)))

  inline def reduce(identity: Long)(inline op: (Long, Long) => Long): Long =
    jStream.reduce(identity, jLongBinaryOperator(op))

  inline def reduce(inline op: (Long, Long) => Long): Option[Long] =
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

  def to[C](implicit fac: Factory[Long, C]): C = {
    val b = fac.newBuilder
    forEachOrdered(b += _)
    b.result()
  }

}
