package com.avsystem.commons
package jiop

import java.util as ju

trait JStreamUtils {
  type JBaseStream[T, S <: ju.stream.BaseStream[T, S]] = ju.stream.BaseStream[T, S]
  type JStream[T] = ju.stream.Stream[T]
  type JDoubleStream = ju.stream.DoubleStream
  type JIntStream = ju.stream.IntStream
  type JLongStream = ju.stream.LongStream
  type JCollector[T, A, R] = ju.stream.Collector[T, A, R]

  extension [T](jStream: JStream[T]) {
    def asScala: ScalaJStream[T] = new ScalaJStream(jStream)
  }

  extension (jStream: JStream[Int]) {
    def asScalaIntStream: ScalaJIntStream = jStream.asScala.asIntStream
  }

  extension (jStream: JStream[Long]) {
    def asScalaLongStream: ScalaJLongStream = jStream.asScala.asLongStream
  }

  extension (jStream: JStream[Double]) {
    def asScalaDoubleStream: ScalaJDoubleStream = jStream.asScala.asDoubleStream
  }

  extension (jStream: JDoubleStream) {
    def asScala: ScalaJDoubleStream = new ScalaJDoubleStream(jStream)
  }

  extension (jStream: JIntStream) {
    def asScala: ScalaJIntStream = new ScalaJIntStream(jStream)
  }

  extension (jStream: JLongStream) {
    def asScala: ScalaJLongStream = new ScalaJLongStream(jStream)
  }
}

object JStreamUtils extends JStreamUtils
