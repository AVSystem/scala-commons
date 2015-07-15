package com.avsystem.commons
package jiop

import java.{util => ju}

trait JStreamUtils {
  type JBaseStream[T, S <: JBaseStream[T, S]] = ju.stream.BaseStream[T, S]
  type JStream[T] = ju.stream.Stream[T]
  type JDoubleStream = ju.stream.DoubleStream
  type JIntStream = ju.stream.IntStream
  type JLongStream = ju.stream.LongStream
  type JCollector[T, A, R] = ju.stream.Collector[T, A, R]

  import JStreamUtils._

  @inline implicit def jStream2AsScala[T](jStream: JStream[T]): JStream2AsScala[T] =
    new JStream2AsScala(jStream)

  @inline implicit def jDoubleStream2AsScala(jStream: JDoubleStream): JDoubleStream2AsScala =
    new JDoubleStream2AsScala(jStream)

  @inline implicit def jIntStream2AsScala(jStream: JIntStream): JIntStream2AsScala =
    new JIntStream2AsScala(jStream)

  @inline implicit def jLongStream2AsScala(jStream: JLongStream): JLongStream2AsScala =
    new JLongStream2AsScala(jStream)
}

object JStreamUtils {

  import JavaInterop._

  final class JStream2AsScala[T](private val jStream: JStream[T]) extends AnyVal {
    @inline def asScala: ScalaJStream[T] = new ScalaJStream(jStream)
  }

  final class JDoubleStream2AsScala(private val jStream: JDoubleStream) extends AnyVal {
    @inline def asScala: ScalaJDoubleStream = new ScalaJDoubleStream(jStream)
  }

  final class JIntStream2AsScala(private val jStream: JIntStream) extends AnyVal {
    @inline def asScala: ScalaJIntStream = new ScalaJIntStream(jStream)
  }

  final class JLongStream2AsScala(private val jStream: JLongStream) extends AnyVal {
    @inline def asScala: ScalaJLongStream = new ScalaJLongStream(jStream)
  }

}
