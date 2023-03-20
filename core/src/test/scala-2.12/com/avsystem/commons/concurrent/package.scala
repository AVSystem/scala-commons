package com.avsystem.commons

import scala.collection.generic.{CanBuildFrom, MapFactory}
import scala.collection.mutable

package object concurrent {
  //workaround for compat issues with 2.12 in ObservableExtensionsTest
  implicit def mutableMapFactoryToCBF[K, V, CC[A, B] <: MMap[A, B] with mutable.MapLike[A, B, CC[A, B]]](
    fact: MapFactory[CC]
  ): CanBuildFrom[Any, (K, V), CC[K, V]] =
    new CanBuildFrom[Any, (K, V), CC[K, V]] {
      def apply(from: Any): mutable.Builder[(K, V), CC[K, V]] = apply()
      def apply(): mutable.Builder[(K, V), CC[K, V]] = fact.newBuilder[K, V]
    }
}
