package com.avsystem.commons
package jiop

import com.avsystem.commons.collection.{CrossBuilder, CrossFactory}

import scala.collection.mutable

trait JFactory[-Elem, +To] extends CrossFactory[Elem, To] {
  def fromSpecific(it: IterableOnce[Elem]): To = {
    val b = this.newBuilder
    b ++= it
    b.result()
  }
}
object JFactory extends JFactories

final class JCollectionFactory[A, C <: JCollection[A]](creator: => C) extends JFactory[A, C] {
  def newBuilder: mutable.Builder[A, C] = new CrossBuilder[A, C] {
    private val coll = creator

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

final class JMapFactory[K, V, M <: JMap[K, V]](creator: => M) extends JFactory[(K, V), M] {
  def newBuilder: MBuilder[(K, V), M] = new CrossBuilder[(K, V), M] {
    private val map = creator

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

trait JFactories extends JCollectionFac with JMapFac

trait JCollectionFac extends JIterableFac

trait JIterableFac extends JSetFac with JLinkedListFac {
  // for JIterable, JCollection, JList and JArrayList
  implicit def jArrayListFac[A]: JCollectionFactory[A, JArrayList[A]] =
    new JCollectionFactory(new JArrayList)
}

trait JSetFac extends JSortedSetFac with JLinkedHashSetFac {
  // for JSet and JHashSet
  implicit def jHashSetFac[A]: JCollectionFactory[A, JHashSet[A]] =
    new JCollectionFactory(new JHashSet)
}

trait JLinkedHashSetFac {
  implicit def jLinkedHashSetFac[A]: JCollectionFactory[A, JLinkedHashSet[A]] =
    new JCollectionFactory(new JLinkedHashSet)
}

trait JSortedSetFac {
  // for JSortedSet, JNavigableSet and JTreeSet
  implicit def jTreeSetFac[A: Ordering]: JCollectionFactory[A, JTreeSet[A]] =
    new JCollectionFactory(new JTreeSet(Ordering[A]))
}

trait JLinkedListFac {
  implicit def jLinkedListFac[A]: JCollectionFactory[A, JLinkedList[A]] =
    new JCollectionFactory(new JLinkedList)
}

trait JMapFac extends JSortedMapFac with JLinkedHashMapFac {
  // for JMap and JHashMap
  implicit def jHashMapFac[K, V]: JMapFactory[K, V, JHashMap[K, V]] =
    new JMapFactory(new JHashMap)
}

trait JLinkedHashMapFac {
  implicit def jLinkedHashMapFac[K, V]: JMapFactory[K, V, JLinkedHashMap[K, V]] =
    new JMapFactory(new JLinkedHashMap)
}

trait JSortedMapFac {
  // for JSortedMap, JNavigableMap and JTreeMap
  implicit def jTreeMapFac[K: Ordering, V]: JMapFactory[K, V, JTreeMap[K, V]] =
    new JMapFactory(new JTreeMap(Ordering[K]))
}
