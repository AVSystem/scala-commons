package com.avsystem.commons
package jiop

import java.util.{DoubleSummaryStatistics, Spliterator}

import com.avsystem.commons.jiop.JavaInterop._

/**
 * Author: ghik
 * Created: 15/07/15.
 */
final class ScalaJDoubleStream(val asJava: JDoubleStream) extends AnyVal {
  def close(): Unit =
    asJava.close()

  def spliterator: Spliterator[Double] =
    asJava.spliterator().asInstanceOf[Spliterator[Double]]

  def isParallel: Boolean =
    asJava.isParallel

  def iterator: Iterator[Double] =
    asJava.iterator().asInstanceOf[JIterator[Double]].asScala

  def onClose(closeHandler: => Any): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.parallel())

  def sequential: ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.sequential())

  def unordered: ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.unordered())

  def allMatch(predicate: Double => Boolean): Boolean =
    asJava.allMatch(jDoublePredicate(predicate))

  def anyMatch(predicate: Double => Boolean): Boolean =
    asJava.allMatch(jDoublePredicate(predicate))

  def average: Option[Double] =
    asJava.average.asScala

  def boxed: ScalaJStream[Double] =
    new ScalaJStream(asJava.boxed.asInstanceOf[JStream[Double]])

  def collect[R](supplier: => R)(accumulator: (R, Double) => Any, combiner: (R, R) => Any): R =
    asJava.collect(jSupplier(supplier), jObjDoubleConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    asJava.count

  def distinct: ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.distinct)

  def filter(predicate: Double => Boolean): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.filter(jDoublePredicate(predicate)))

  def findAny: Option[Double] =
    asJava.findAny().asScala

  def findFirst: Option[Double] =
    asJava.findFirst.asScala

  def flatMap(mapper: Double => ScalaJDoubleStream): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.flatMap(jDoubleFunction(d => mapper(d).asJava)))

  def forEach(action: Double => Any): Unit =
    asJava.forEach(jDoubleConsumer(action))

  def forEachOrdered(action: Double => Any): Unit =
    asJava.forEachOrdered(jDoubleConsumer(action))

  def limit(maxSize: Long): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.limit(maxSize))

  def map(mapper: Double => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.map(jDoubleUnaryOperator(mapper)))

  def mapToInt(mapper: Double => Int): ScalaJIntStream =
    new ScalaJIntStream(asJava.mapToInt(jDoubleToIntFunction(mapper)))

  def mapToLong(mapper: Double => Long): ScalaJLongStream =
    new ScalaJLongStream(asJava.mapToLong(jDoubleToLongFunction(mapper)))

  def mapToObj[U](mapper: Double => U): ScalaJStream[U] =
    new ScalaJStream(asJava.mapToObj(jDoubleFunction(mapper)))

  def max: Option[Double] =
    asJava.max.asScala

  def min: Option[Double] =
    asJava.min.asScala

  def noneMatch(predicate: Double => Boolean): Boolean =
    asJava.noneMatch(jDoublePredicate(predicate))

  def peek(action: Double => Any): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.peek(jDoubleConsumer(action)))

  def reduce(identity: Double)(op: (Double, Double) => Double): Double =
    asJava.reduce(identity, jDoubleBinaryOperator(op))

  def reduce(op: (Double, Double) => Double): Option[Double] =
    asJava.reduce(jDoubleBinaryOperator(op)).asScala

  def skip(n: Long): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.skip(n))

  def sorted: ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.sorted)

  def sum: Double =
    asJava.sum

  def summaryStatistics: DoubleSummaryStatistics =
    asJava.summaryStatistics()

  def toArray: Array[Double] =
    asJava.toArray

}
