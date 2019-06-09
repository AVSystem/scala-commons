package com.avsystem.commons
package jiop

import scala.collection.{Factory, mutable}

trait JBuildFrom[-Elem, +To] extends Factory[Elem, To] {
  def fromSpecific(it: IterableOnce[Elem]): To = {
    val b = newBuilder
    b.addAll(it)
    b.result()
  }
}
object JBuildFrom extends JBuildFroms

final class JCollectionCBF[A, C <: JCollection[A]](creator: => C) extends JBuildFrom[A, C] {
  def newBuilder = new mutable.Builder[A, C] {
    val coll = creator

    def addOne(elem: A): this.type = {
      coll.add(elem)
      this
    }

    def clear(): Unit =
      coll.clear()

    def result(): C =
      coll
  }
}

final class JMapCBF[K, V, M <: JMap[K, V]](creator: => M) extends JBuildFrom[(K, V), M] {
  def newBuilder = new mutable.Builder[(K, V), M] {
    val map = creator

    def addOne(elem: (K, V)): this.type = {
      map.put(elem._1, elem._2)
      this
    }

    def clear(): Unit =
      map.clear()

    def result(): M =
      map
  }
}

trait JBuildFroms extends JCollectionBuildFroms with JMapBuildFroms

trait JCollectionBuildFroms extends JIterableCBF

trait JIterableCBF extends JSetCBF with JLinkedListCBF {
  // for JIterable, JCollection, JList and JArrayList
  implicit def jArrayListCBF[A]: JCollectionCBF[A, JArrayList[A]] =
    new JCollectionCBF(new JArrayList)
}

trait JSetCBF extends JSortedSetCBF with JLinkedHashSetCBF {
  // for JSet and JHashSet
  implicit def jHashSetCBF[A]: JCollectionCBF[A, JHashSet[A]] =
    new JCollectionCBF(new JHashSet)
}

trait JLinkedHashSetCBF {
  implicit def jLinkedHashSetCBF[A]: JCollectionCBF[A, JLinkedHashSet[A]] =
    new JCollectionCBF(new JLinkedHashSet)
}

trait JSortedSetCBF {
  // for JSortedSet, JNavigableSet and JTreeSet
  implicit def jTreeSetCBF[A: Ordering]: JCollectionCBF[A, JTreeSet[A]] =
    new JCollectionCBF(new JTreeSet(Ordering[A]))
}

trait JLinkedListCBF {
  implicit def jLinkedListCBF[A]: JCollectionCBF[A, JLinkedList[A]] =
    new JCollectionCBF(new JLinkedList)
}

trait JMapBuildFroms extends JSortedMapCBF with JLinkedHashMapCBF {
  // for JMap and JHashMap
  implicit def jHashMapCBF[K, V]: JMapCBF[K, V, JHashMap[K, V]] =
    new JMapCBF(new JHashMap)
}

trait JLinkedHashMapCBF {
  implicit def jLinkedHashMapCBF[K, V]: JMapCBF[K, V, JLinkedHashMap[K, V]] =
    new JMapCBF(new JLinkedHashMap)
}

trait JSortedMapCBF {
  // for JSortedMap, JNavigableMap and JTreeMap
  implicit def jTreeMapCBF[K: Ordering, V]: JMapCBF[K, V, JTreeMap[K, V]] =
    new JMapCBF(new JTreeMap(Ordering[K]))
}
