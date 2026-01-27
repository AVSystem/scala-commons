package com.avsystem.commons
package jiop

import java.util.DoubleSummaryStatistics
import scala.collection.Factory

final class ScalaJDoubleStream(private val jStream: JDoubleStream) extends AnyVal {
  def asJava: JDoubleStream = jStream

  def close(): Unit =
    jStream.close()

  def isParallel: Boolean =
    jStream.isParallel

  def iterator: Iterator[Double] =
    jStream.iterator().asInstanceOf[JIterator[Double]].asScala

  def onClose(closeHandler: => Any): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.parallel())

  def sequential: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.sequential())

  def unordered: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.unordered())

  def allMatch(predicate: Double => Boolean): Boolean =
    jStream.allMatch(jDoublePredicate(predicate))

  def anyMatch(predicate: Double => Boolean): Boolean =
    jStream.anyMatch(jDoublePredicate(predicate))

  def average: Option[Double] =
    jStream.average.asScala

  def boxed: ScalaJStream[Double] =
    new ScalaJStream(jStream.boxed.asInstanceOf[JStream[Double]])

  def collect[R](supplier: => R)(accumulator: (R, Double) => Any, combiner: (R, R) => Any): R =
    jStream.collect(jSupplier(supplier), jObjDoubleConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    jStream.count

  def distinct: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.distinct)

  def filter(predicate: Double => Boolean): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.filter(jDoublePredicate(predicate)))

  def findAny: Option[Double] =
    jStream.findAny().asScala

  def findFirst: Option[Double] =
    jStream.findFirst.asScala

  def flatMap(mapper: Double => ScalaJDoubleStream): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.flatMap(jDoubleFunction(d => mapper(d).jStream)))

  def forEach(action: Double => Any): Unit =
    jStream.forEach(jDoubleConsumer(action))

  def forEachOrdered(action: Double => Any): Unit =
    jStream.forEachOrdered(jDoubleConsumer(action))

  def limit(maxSize: Long): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.limit(maxSize))

  def map(mapper: Double => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.map(jDoubleUnaryOperator(mapper)))

  def mapToInt(mapper: Double => Int): ScalaJIntStream =
    new ScalaJIntStream(jStream.mapToInt(jDoubleToIntFunction(mapper)))

  def mapToLong(mapper: Double => Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.mapToLong(jDoubleToLongFunction(mapper)))

  def mapToObj[U](mapper: Double => U): ScalaJStream[U] =
    new ScalaJStream(jStream.mapToObj(jDoubleFunction(mapper)))

  def max: Option[Double] =
    jStream.max.asScala

  def min: Option[Double] =
    jStream.min.asScala

  def noneMatch(predicate: Double => Boolean): Boolean =
    jStream.noneMatch(jDoublePredicate(predicate))

  def peek(action: Double => Any): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.peek(jDoubleConsumer(action)))

  def reduce(identity: Double)(op: (Double, Double) => Double): Double =
    jStream.reduce(identity, jDoubleBinaryOperator(op))

  def reduce(op: (Double, Double) => Double): Option[Double] =
    jStream.reduce(jDoubleBinaryOperator(op)).asScala

  def skip(n: Long): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.skip(n))

  def sorted: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.sorted)

  def sum: Double =
    jStream.sum

  def summaryStatistics: DoubleSummaryStatistics =
    jStream.summaryStatistics()

  def toArray: Array[Double] =
    jStream.toArray

  def to[C](using fac: Factory[Double, C]): C = {
    val b = fac.newBuilder
    forEachOrdered(b += _)
    b.result()
  }

}
