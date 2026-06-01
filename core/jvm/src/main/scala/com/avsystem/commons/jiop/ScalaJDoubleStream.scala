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

  inline def onClose(closeHandler: => Any): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.parallel())

  def sequential: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.sequential())

  def unordered: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.unordered())

  inline def allMatch(inline predicate: Double => Boolean): Boolean =
    jStream.allMatch(jDoublePredicate(predicate))

  inline def anyMatch(inline predicate: Double => Boolean): Boolean =
    jStream.anyMatch(jDoublePredicate(predicate))

  def average: Option[Double] =
    jStream.average.asScala

  def boxed: ScalaJStream[Double] =
    new ScalaJStream(jStream.boxed.asInstanceOf[JStream[Double]])

  inline def collect[R](supplier: => R)(inline accumulator: (R, Double) => Any, inline combiner: (R, R) => Any): R =
    jStream.collect(jSupplier(supplier), jObjDoubleConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    jStream.count

  def distinct: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.distinct)

  inline def filter(inline predicate: Double => Boolean): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.filter(jDoublePredicate(predicate)))

  def findAny: Option[Double] =
    jStream.findAny().asScala

  def findFirst: Option[Double] =
    jStream.findFirst.asScala

  inline def flatMap(inline mapper: Double => ScalaJDoubleStream): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.flatMap(jDoubleFunction(d => mapper(d).jStream)))

  inline def forEach(inline action: Double => Any): Unit =
    jStream.forEach(jDoubleConsumer(action))

  inline def forEachOrdered(inline action: Double => Any): Unit =
    jStream.forEachOrdered(jDoubleConsumer(action))

  def limit(maxSize: Long): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.limit(maxSize))

  inline def map(inline mapper: Double => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.map(jDoubleUnaryOperator(mapper)))

  inline def mapToInt(inline mapper: Double => Int): ScalaJIntStream =
    new ScalaJIntStream(jStream.mapToInt(jDoubleToIntFunction(mapper)))

  inline def mapToLong(inline mapper: Double => Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.mapToLong(jDoubleToLongFunction(mapper)))

  inline def mapToObj[U](inline mapper: Double => U): ScalaJStream[U] =
    new ScalaJStream(jStream.mapToObj(jDoubleFunction(mapper)))

  def max: Option[Double] =
    jStream.max.asScala

  def min: Option[Double] =
    jStream.min.asScala

  inline def noneMatch(inline predicate: Double => Boolean): Boolean =
    jStream.noneMatch(jDoublePredicate(predicate))

  inline def peek(inline action: Double => Any): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.peek(jDoubleConsumer(action)))

  inline def reduce(identity: Double)(inline op: (Double, Double) => Double): Double =
    jStream.reduce(identity, jDoubleBinaryOperator(op))

  inline def reduce(inline op: (Double, Double) => Double): Option[Double] =
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

  def to[C](implicit fac: Factory[Double, C]): C = {
    val b = fac.newBuilder
    forEachOrdered(b += _)
    b.result()
  }

}
