package com.avsystem.commons
package jiop

import java.util.IntSummaryStatistics

import com.avsystem.commons.jiop.JavaInterop._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

final class ScalaJIntStream(private val jStream: JIntStream) extends AnyVal {
  def asJava = jStream

  def close(): Unit =
    jStream.close()

  def isParallel: Boolean =
    jStream.isParallel

  def iterator: Iterator[Int] =
    jStream.iterator().asInstanceOf[JIterator[Int]].asScala

  def onClose(closeHandler: => Any): ScalaJIntStream =
    new ScalaJIntStream(jStream.onClose(jRunnable(closeHandler)))

  def parallel: ScalaJIntStream =
    new ScalaJIntStream(jStream.parallel())

  def sequential: ScalaJIntStream =
    new ScalaJIntStream(jStream.sequential())

  def unordered: ScalaJIntStream =
    new ScalaJIntStream(jStream.unordered())

  def allMatch(predicate: Int => Boolean): Boolean =
    jStream.allMatch(jIntPredicate(predicate))

  def anyMatch(predicate: Int => Boolean): Boolean =
    jStream.allMatch(jIntPredicate(predicate))

  def asDoubleStream: ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.asDoubleStream())

  def asLongStream: ScalaJLongStream =
    new ScalaJLongStream(jStream.asLongStream())

  def average: Option[Double] =
    jStream.average.asScala

  def boxed: ScalaJStream[Int] =
    new ScalaJStream(jStream.boxed.asInstanceOf[JStream[Int]])

  def collect[R](supplier: => R)(accumulator: (R, Int) => Any, combiner: (R, R) => Any): R =
    jStream.collect(jSupplier(supplier), jObjIntConsumer(accumulator), jBiConsumer(combiner))

  def count: Long =
    jStream.count

  def distinct: ScalaJIntStream =
    new ScalaJIntStream(jStream.distinct)

  def filter(predicate: Int => Boolean): ScalaJIntStream =
    new ScalaJIntStream(jStream.filter(jIntPredicate(predicate)))

  def findAny: Option[Int] =
    jStream.findAny().asScala

  def findFirst: Option[Int] =
    jStream.findFirst.asScala

  def flatMap(mapper: Int => ScalaJIntStream): ScalaJIntStream =
    new ScalaJIntStream(jStream.flatMap(jIntFunction(d => mapper(d).jStream)))

  def forEach(action: Int => Any): Unit =
    jStream.forEach(jIntConsumer(action))

  def forEachOrdered(action: Int => Any): Unit =
    jStream.forEachOrdered(jIntConsumer(action))

  def limit(maxSize: Long): ScalaJIntStream =
    new ScalaJIntStream(jStream.limit(maxSize))

  def map(mapper: Int => Int): ScalaJIntStream =
    new ScalaJIntStream(jStream.map(jIntUnaryOperator(mapper)))

  def mapToDouble(mapper: Int => Double): ScalaJDoubleStream =
    new ScalaJDoubleStream(jStream.mapToDouble(jIntToDoubleFunction(mapper)))

  def mapToLong(mapper: Int => Long): ScalaJLongStream =
    new ScalaJLongStream(jStream.mapToLong(jIntToLongFunction(mapper)))

  def mapToObj[U](mapper: Int => U): ScalaJStream[U] =
    new ScalaJStream(jStream.mapToObj(jIntFunction(mapper)))

  def max: Option[Int] =
    jStream.max.asScala

  def min: Option[Int] =
    jStream.min.asScala

  def noneMatch(predicate: Int => Boolean): Boolean =
    jStream.noneMatch(jIntPredicate(predicate))

  def peek(action: Int => Any): ScalaJIntStream =
    new ScalaJIntStream(jStream.peek(jIntConsumer(action)))

  def reduce(identity: Int)(op: (Int, Int) => Int): Int =
    jStream.reduce(identity, jIntBinaryOperator(op))

  def reduce(op: (Int, Int) => Int): Option[Int] =
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

  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, Int, Col[Int]]): Col[Int] = {
    val b = cbf.apply()
    forEachOrdered(b += _)
    b.result()
  }

}