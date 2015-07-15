package com.avsystem.commons
package jiop

import java.util.{IntSummaryStatistics, Spliterator}

import com.avsystem.commons.jiop.JavaInterop._

/**
 * Author: ghik
 * Created: 15/07/15.
 */
final class ScalaJIntStream(val asJava: JIntStream) extends AnyVal {
  def close(): Unit =
    asJava.close()

  def spliterator: Spliterator[Int] =
    asJava.spliterator().asInstanceOf[Spliterator[Int]]

  def isParallel: Boolean =
    asJava.isParallel

  def iterator: Iterator[Int] =
    asJava.iterator().asInstanceOf[JIterator[Int]].asScala

  def onClose(closeHandler: => Any): ScalaJIntStream =
    new ScalaJIntStream(asJava.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJIntStream =
    new ScalaJIntStream(asJava.parallel())

  def sequential: ScalaJIntStream =
    new ScalaJIntStream(asJava.sequential())

  def unordered: ScalaJIntStream =
    new ScalaJIntStream(asJava.unordered())

  def allMatch(predicate: Int => Boolean): Boolean =
    asJava.allMatch(jIntPredicate(predicate))

  def anyMatch(predicate: Int => Boolean): Boolean =
    asJava.allMatch(jIntPredicate(predicate))

  def asDoubleStream: ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.asDoubleStream())

  def asLongStream: ScalaJLongStream =
    new ScalaJLongStream(asJava.asLongStream())

  def average: Option[Double] =
    asJava.average.asScala

  def boxed: ScalaJStream[Int] =
    new ScalaJStream(asJava.boxed.asInstanceOf[JStream[Int]])

  def collect[R](supplier: => R)(accumulator: (R, Int) => Any, combiner: (R, R) => Any): R =
    asJava.collect(jSupplier(supplier), jObjIntConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    asJava.count

  def distinct: ScalaJIntStream =
    new ScalaJIntStream(asJava.distinct)

  def filter(predicate: Int => Boolean): ScalaJIntStream =
    new ScalaJIntStream(asJava.filter(jIntPredicate(predicate)))

  def findAny: Option[Int] =
    asJava.findAny().asScala

  def findFirst: Option[Int] =
    asJava.findFirst.asScala

  def flatMap(mapper: Int => ScalaJIntStream): ScalaJIntStream =
    new ScalaJIntStream(asJava.flatMap(jIntFunction(d => mapper(d).asJava)))

  def forEach(action: Int => Any): Unit =
    asJava.forEach(jIntConsumer(action))

  def forEachOrdered(action: Int => Any): Unit =
    asJava.forEachOrdered(jIntConsumer(action))

  def limit(maxSize: Long): ScalaJIntStream =
    new ScalaJIntStream(asJava.limit(maxSize))

  def map(mapper: Int => Int): ScalaJIntStream =
    new ScalaJIntStream(asJava.map(jIntUnaryOperator(mapper)))

  def mapToDouble(mapper: Int => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(asJava.mapToDouble(jIntToDoubleFunction(mapper)))

  def mapToLong(mapper: Int => Long): ScalaJLongStream =
    new ScalaJLongStream(asJava.mapToLong(jIntToLongFunction(mapper)))

  def mapToObj[U](mapper: Int => U): ScalaJStream[U] =
    new ScalaJStream(asJava.mapToObj(jIntFunction(mapper)))

  def max: Option[Int] =
    asJava.max.asScala

  def min: Option[Int] =
    asJava.min.asScala

  def noneMatch(predicate: Int => Boolean): Boolean =
    asJava.noneMatch(jIntPredicate(predicate))

  def peek(action: Int => Any): ScalaJIntStream =
    new ScalaJIntStream(asJava.peek(jIntConsumer(action)))

  def reduce(identity: Int)(op: (Int, Int) => Int): Int =
    asJava.reduce(identity, jIntBinaryOperator(op))

  def reduce(op: (Int, Int) => Int): Option[Int] =
    asJava.reduce(jIntBinaryOperator(op)).asScala

  def skip(n: Long): ScalaJIntStream =
    new ScalaJIntStream(asJava.skip(n))

  def sorted: ScalaJIntStream =
    new ScalaJIntStream(asJava.sorted)

  def sum: Int =
    asJava.sum

  def summaryStatistics: IntSummaryStatistics =
    asJava.summaryStatistics()

  def toArray: Array[Int] =
    asJava.toArray

}
