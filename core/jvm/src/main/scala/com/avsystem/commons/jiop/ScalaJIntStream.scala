package com.avsystem.commons
package jiop

import java.util.IntSummaryStatistics
import scala.collection.Factory

final class ScalaJIntStream(private val jStream: JIntStream) extends AnyVal {
  def asJava: JIntStream = jStream

  def close(): Unit =
    jStream.close()

  def isParallel: Boolean =
    jStream.isParallel

  def iterator: Iterator[Int] =
    jStream.iterator().asInstanceOf[JIterator[Int]].asScala

  inline def onClose(closeHandler: => Any): ScalaJIntStream =
    new ScalaJIntStream(jStream.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJIntStream =
    new ScalaJIntStream(jStream.parallel())

  def sequential: ScalaJIntStream =
    new ScalaJIntStream(jStream.sequential())

  def unordered: ScalaJIntStream =
    new ScalaJIntStream(jStream.unordered())

  inline def allMatch(inline predicate: Int => Boolean): Boolean =
    jStream.allMatch(jIntPredicate(predicate))

  inline def anyMatch(inline predicate: Int => Boolean): Boolean =
    jStream.anyMatch(jIntPredicate(predicate))

  def asDoubleStream: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.asDoubleStream())

  def asLongStream: ScalaJLongStream =
    new ScalaJLongStream(jStream.asLongStream())

  def average: Option[Double] =
    jStream.average.asScala

  def boxed: ScalaJStream[Int] =
    new ScalaJStream(jStream.boxed.asInstanceOf[JStream[Int]])

  inline def collect[R](supplier: => R)(inline accumulator: (R, Int) => Any, inline combiner: (R, R) => Any): R =
    jStream.collect(jSupplier(supplier), jObjIntConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    jStream.count

  def distinct: ScalaJIntStream =
    new ScalaJIntStream(jStream.distinct)

  inline def filter(inline predicate: Int => Boolean): ScalaJIntStream =
    new ScalaJIntStream(jStream.filter(jIntPredicate(predicate)))

  def findAny: Option[Int] =
    jStream.findAny().asScala

  def findFirst: Option[Int] =
    jStream.findFirst.asScala

  inline def flatMap(inline mapper: Int => ScalaJIntStream): ScalaJIntStream =
    new ScalaJIntStream(jStream.flatMap(jIntFunction(d => mapper(d).jStream)))

  inline def forEach(inline action: Int => Any): Unit =
    jStream.forEach(jIntConsumer(action))

  inline def forEachOrdered(inline action: Int => Any): Unit =
    jStream.forEachOrdered(jIntConsumer(action))

  def limit(maxSize: Long): ScalaJIntStream =
    new ScalaJIntStream(jStream.limit(maxSize))

  inline def map(inline mapper: Int => Int): ScalaJIntStream =
    new ScalaJIntStream(jStream.map(jIntUnaryOperator(mapper)))

  inline def mapToDouble(inline mapper: Int => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.mapToDouble(jIntToDoubleFunction(mapper)))

  inline def mapToLong(inline mapper: Int => Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.mapToLong(jIntToLongFunction(mapper)))

  inline def mapToObj[U](inline mapper: Int => U): ScalaJStream[U] =
    new ScalaJStream(jStream.mapToObj(jIntFunction(mapper)))

  def max: Option[Int] =
    jStream.max.asScala

  def min: Option[Int] =
    jStream.min.asScala

  inline def noneMatch(inline predicate: Int => Boolean): Boolean =
    jStream.noneMatch(jIntPredicate(predicate))

  inline def peek(inline action: Int => Any): ScalaJIntStream =
    new ScalaJIntStream(jStream.peek(jIntConsumer(action)))

  inline def reduce(identity: Int)(inline op: (Int, Int) => Int): Int =
    jStream.reduce(identity, jIntBinaryOperator(op))

  inline def reduce(inline op: (Int, Int) => Int): Option[Int] =
    jStream.reduce(jIntBinaryOperator(op)).asScala

  def skip(n: Long): ScalaJIntStream =
    new ScalaJIntStream(jStream.skip(n))

  def sorted: ScalaJIntStream =
    new ScalaJIntStream(jStream.sorted)

  def sum: Int =
    jStream.sum

  def summaryStatistics: IntSummaryStatistics =
    jStream.summaryStatistics()

  def toArray: Array[Int] =
    jStream.toArray

  def to[C](implicit fac: Factory[Int, C]): C = {
    val b = fac.newBuilder
    forEachOrdered(b += _)
    b.result()
  }

}
