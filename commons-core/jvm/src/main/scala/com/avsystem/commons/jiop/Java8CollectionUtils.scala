package com.avsystem.commons
package jiop

import com.avsystem.commons.jiop.JavaInterop._

trait Java8CollectionUtils {

  import Java8CollectionUtils._

  implicit def jIteratorOps[A](it: JIterator[A]): jIteratorOps[A] = new jIteratorOps(it)
  implicit def jIterableOps[A](it: JIterable[A]): jIterableOps[A] = new jIterableOps(it)
  implicit def jCollectionOps[A](it: JCollection[A]): jCollectionOps[A] = new jCollectionOps(it)
  implicit def intJCollectionOps(it: JCollection[Int]): intJCollectionOps = new intJCollectionOps(it)
  implicit def longJCollectionOps(it: JCollection[Long]): longJCollectionOps = new longJCollectionOps(it)
  implicit def doubleJCollectionOps(it: JCollection[Double]): doubleJCollectionOps = new doubleJCollectionOps(it)
  implicit def jMapOps[K, V](map: JMap[K, V]): jMapOps[K, V] = new jMapOps(map)
}

object Java8CollectionUtils {
  class jIteratorOps[A](private val it: JIterator[A]) extends AnyVal {
    def forEachRemaining(code: A => Any): Unit =
      it.forEachRemaining(jConsumer(code))
  }

  class jIterableOps[A](private val it: JIterable[A]) extends AnyVal {
    def forEach(code: A => Any): Unit =
      it.forEach(jConsumer(code))
  }

  class jCollectionOps[A](private val coll: JCollection[A]) extends AnyVal {
    def removeIf(pred: A => Boolean): Unit =
      coll.removeIf(jPredicate(pred))

    def scalaStream: ScalaJStream[A] =
      coll.stream.asScala
  }

  class intJCollectionOps(private val coll: JCollection[Int]) extends AnyVal {
    def scalaIntStream: ScalaJIntStream =
      coll.stream.asScalaIntStream
  }

  class longJCollectionOps(private val coll: JCollection[Long]) extends AnyVal {
    def scalaLongStream: ScalaJLongStream =
      coll.stream.asScalaLongStream
  }

  class doubleJCollectionOps(private val coll: JCollection[Double]) extends AnyVal {
    def scalaDoubleStream: ScalaJDoubleStream =
      coll.stream.asScalaDoubleStream
  }

  class jMapOps[K, V](private val map: JMap[K, V]) extends AnyVal {
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
