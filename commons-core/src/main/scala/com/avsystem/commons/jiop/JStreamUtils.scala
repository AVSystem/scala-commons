package com.avsystem.commons
package jiop

import java.util.Collections
import java.{util => ju}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

trait JStreamUtils {
  type JBaseStream[T, S <: JBaseStream[T, S]] = ju.stream.BaseStream[T, S]
  type JStream[T] = ju.stream.Stream[T]
  type JDoubleStream = ju.stream.DoubleStream
  type JIntStream = ju.stream.IntStream
  type JLongStream = ju.stream.LongStream
  type JCollector[T, A, R] = ju.stream.Collector[T, A, R]

  import JStreamUtils._

  implicit def jStream2AsScala[T](jStream: JStream[T]): JStream2AsScala[T] =
    new JStream2AsScala(jStream)

  implicit def jStream2AsScalaIntStream(jStream: JStream[Int]): JStream2AsScalaIntStream =
    new JStream2AsScalaIntStream(jStream)

  implicit def jStream2AsScalaLongStream(jStream: JStream[Long]): JStream2AsScalaLongStream =
    new JStream2AsScalaLongStream(jStream)

  implicit def jStream2AsScalaDoubleStream(jStream: JStream[Double]): JStream2AsScalaDoubleStream =
    new JStream2AsScalaDoubleStream(jStream)

  implicit def jDoubleStream2AsScala(jStream: JDoubleStream): JDoubleStream2AsScala =
    new JDoubleStream2AsScala(jStream)

  implicit def jIntStream2AsScala(jStream: JIntStream): JIntStream2AsScala =
    new JIntStream2AsScala(jStream)

  implicit def jLongStream2AsScala(jStream: JLongStream): JLongStream2AsScala =
    new JLongStream2AsScala(jStream)
}

object JStreamUtils {

  import JavaInterop._

  final class JStream2AsScala[T](private val jStream: JStream[T]) extends AnyVal {
    def asScala: ScalaJStream[T] = new ScalaJStream(jStream)
  }

  final class JStream2AsScalaIntStream(private val jStream: JStream[Int]) extends AnyVal {
    def asScalaIntStream: ScalaJIntStream = jStream.asScala.asIntStream
  }

  final class JStream2AsScalaLongStream(private val jStream: JStream[Long]) extends AnyVal {
    def asScalaLongStream: ScalaJLongStream = jStream.asScala.asLongStream
  }

  final class JStream2AsScalaDoubleStream(private val jStream: JStream[Double]) extends AnyVal {
    def asScalaDoubleStream: ScalaJDoubleStream = jStream.asScala.asDoubleStream
  }

  final class JDoubleStream2AsScala(private val jStream: JDoubleStream) extends AnyVal {
    def asScala: ScalaJDoubleStream = new ScalaJDoubleStream(jStream)
  }

  final class JIntStream2AsScala(private val jStream: JIntStream) extends AnyVal {
    def asScala: ScalaJIntStream = new ScalaJIntStream(jStream)
  }

  final class JLongStream2AsScala(private val jStream: JLongStream) extends AnyVal {
    def asScala: ScalaJLongStream = new ScalaJLongStream(jStream)
  }

  /**
   * A [[java.util.stream.Collector]] implementation based on [[CanBuildFrom]] instance.
   */
  final class CanBuildFromCollector[Elem, To](cbf: CanBuildFrom[Nothing, Elem, To])(implicit ato: AsTraversableOnce[To, Elem])
    extends JCollector[Elem, mutable.Builder[Elem, To], To] {

    def accumulator() = jBiConsumer(_ += _)
    def characteristics() = Collections.emptySet()
    def combiner() = jBinaryOperator { (b1, b2) =>
      b1 ++= ato.asTraversableOnce(b2.result())
    }
    def finisher() = jFunction(_.result())
    def supplier() = jSupplier(cbf.apply())
  }

  trait AsTraversableOnce[C, A] {
    def asTraversableOnce(coll: C): TraversableOnce[A]
  }

  object AsTraversableOnce {
    implicit def trivialAsTraversableOnce[A]: AsTraversableOnce[TraversableOnce[A], A] =
      new AsTraversableOnce[TraversableOnce[A], A] {
        def asTraversableOnce(coll: TraversableOnce[A]) = coll
      }

    implicit def jIterableAsTraversableOnce[A]: AsTraversableOnce[JIterable[A], A] =
      new AsTraversableOnce[JIterable[A], A] {
        def asTraversableOnce(coll: JIterable[A]) = coll.iterator.asScala
      }

    implicit def jIteratorAsTraversableOnce[A]: AsTraversableOnce[JIterator[A], A] =
      new AsTraversableOnce[JIterator[A], A] {
        def asTraversableOnce(coll: JIterator[A]) = coll.asScala
      }
  }

}
