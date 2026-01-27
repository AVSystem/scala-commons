package com.avsystem.commons
package jiop

trait Java8CollectionUtils {
  extension [A](it: JIterator[A]) {
    def forEachRemaining(code: A => Any): Unit =
      it.forEachRemaining(jConsumer(code))
  }

  extension [A](it: JIterable[A]) {
    def forEach(code: A => Any): Unit =
      it.forEach(jConsumer(code))
  }

  extension [A](coll: JCollection[A]) {
    def removeIf(pred: A => Boolean): Unit =
      coll.removeIf(jPredicate(pred))

    def scalaStream: ScalaJStream[A] =
      coll.stream.asScala
  }

  extension [A](coll: JCollection[Int]) {
    def scalaIntStream: ScalaJIntStream =
      coll.stream.asScalaIntStream
  }

  extension [A](coll: JCollection[Long]) {
    def scalaLongStream: ScalaJLongStream =
      coll.stream.asScalaLongStream
  }

  extension [A](coll: JCollection[Double]) {
    def scalaDoubleStream: ScalaJDoubleStream =
      coll.stream.asScalaDoubleStream
  }

  extension [K, V](map: JMap[K, V]) {
    def compute(key: K, remappingFunction: (K, V) => V): V =
      map.compute(key, jBiFunction(remappingFunction))

    def computeIfAbsent(key: K)(mappingFunction: K => V): V =
      map.computeIfAbsent(key, jFunction(mappingFunction))

    def computeIfPresent(key: K)(remappingFunction: (K, V) => V): V =
      map.computeIfPresent(key, jBiFunction(remappingFunction))

    def forEach(action: (K, V) => Any): Unit =
      map.forEach(jBiConsumer(action))

    def merge(key: K, value: V)(remappingFunction: (V, V) => V): V =
      map.merge(key, value, jBiFunction(remappingFunction))

    def replaceAll(function: (K, V) => V): Unit =
      map.replaceAll(jBiFunction(function))
  }
}

object Java8CollectionUtils extends Java8CollectionUtils
