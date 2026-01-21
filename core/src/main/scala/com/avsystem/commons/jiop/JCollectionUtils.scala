package com.avsystem.commons
package jiop

import java.{lang as jl, util as ju}
import scala.collection.Factory

trait JCollectionUtils extends JFactories {
  type JIterator[T] = ju.Iterator[T]
  type JIterable[T] = jl.Iterable[T]
  type JCollection[T] = ju.Collection[T]
  type JList[T] = ju.List[T]
  type JListIterator[T] = ju.ListIterator[T]
  type JArrayList[T] = ju.ArrayList[T]
  type JLinkedList[T] = ju.LinkedList[T]
  type JSet[T] = ju.Set[T]
  type JHashSet[T] = ju.HashSet[T]
  type JLinkedHashSet[T] = ju.LinkedHashSet[T]
  type JSortedSet[T] = ju.SortedSet[T]
  type JNavigableSet[T] = ju.NavigableSet[T]
  type JTreeSet[T] = ju.TreeSet[T]
  type JEnumSet[E <: Enum[E]] = ju.EnumSet[E]
  type JMap[K, V] = ju.Map[K, V]
  type JHashMap[K, V] = ju.HashMap[K, V]
  type JLinkedHashMap[K, V] = ju.LinkedHashMap[K, V]
  type JSortedMap[K, V] = ju.SortedMap[K, V]
  type JNavigableMap[K, V] = ju.NavigableMap[K, V]
  type JTreeMap[K, V] = ju.TreeMap[K, V]
  type JEnumMap[K <: Enum[K], V] = ju.EnumMap[K, V]
  type JQueue[E] = ju.Queue[E]
  type JDeque[E] = ju.Deque[E]
  type JArrayDeque[E] = ju.ArrayDeque[E]

  abstract class JCollectionCreator[C[T] <: JCollection[T]] {
    protected def instantiate[T]: C[T]

    def apply[T](elems: T*): C[T] = {
      val res = instantiate[T]
      elems.foreach(res.add)
      res
    }

    def empty[T]: C[T] = instantiate[T]
  }
  object JCollectionCreator {
    implicit def asJCollectionFactory[C[X] <: JCollection[X], T](creator: JCollectionCreator[C]): Factory[T, C[T]] =
      new JCollectionFactory(creator.empty[T])
  }

  abstract class JSortedSetCreator[C[T] <: JSortedSet[T]] {
    protected def instantiate[T](ord: Ordering[T]): C[T]

    def empty[T: Ordering]: C[T] = instantiate(Ordering[T])

    def apply[T: Ordering](values: T*): C[T] = {
      val result = instantiate[T](Ordering[T])
      result.addAll(values.asJava)
      result
    }

    def unapplySeq[T](set: C[T]): Option[Seq[T]] =
      Some(set.iterator.asScala.toSeq)
  }
  object JSortedSetCreator {
    implicit def asJSortedSetFactory[C[X] <: JSortedSet[X], T: Ordering](creator: JSortedSetCreator[C])
      : Factory[T, C[T]] =
      new JCollectionFactory[T, C[T]](creator.empty[T])
  }

  abstract class JListCreator[C[T] <: JList[T]] extends JCollectionCreator[C] {
    def unapplySeq[T](list: C[T]): Option[Seq[T]] =
      Some(list.asScala.toSeq)
  }

  object JIterable {
    def apply[T](values: T*): JIterable[T] =
      JArrayList(values*)

    implicit def asJIterableFactory[T](obj: JIterable.type): Factory[T, JIterable[T]] =
      new JCollectionFactory(new JArrayList)
  }

  object JCollection extends JCollectionCreator[JCollection] {
    protected def instantiate[T]: JCollection[T] = new JArrayList[T]
  }

  object JList extends JListCreator[JList] {
    protected def instantiate[T]: JList[T] = new JArrayList[T]
  }

  object JArrayList extends JListCreator[JArrayList] {
    protected def instantiate[T]: JArrayList[T] = new JArrayList[T]
  }

  object JLinkedList extends JListCreator[JLinkedList] {
    protected def instantiate[T]: JLinkedList[T] = new JLinkedList[T]
  }

  object JSet extends JCollectionCreator[JSet] {
    protected def instantiate[T]: JSet[T] = new JHashSet[T]
  }

  object JHashSet extends JCollectionCreator[JHashSet] {
    protected def instantiate[T]: JHashSet[T] = new JHashSet[T]
  }

  object JLinkedHashSet extends JCollectionCreator[JLinkedHashSet] {
    protected def instantiate[T]: JLinkedHashSet[T] = new JLinkedHashSet[T]

    def unapplySeq[T](set: JLinkedHashSet[T]): Option[Seq[T]] =
      Some(set.iterator.asScala.toSeq)
  }

  object JSortedSet extends JSortedSetCreator[JSortedSet] {
    protected def instantiate[T](ord: Ordering[T]): JSortedSet[T] = new JTreeSet[T](ord)
  }

  object JNavigableSet extends JSortedSetCreator[JNavigableSet] {
    protected def instantiate[T](ord: Ordering[T]): JNavigableSet[T] = new JTreeSet[T](ord)
  }

  object JTreeSet extends JSortedSetCreator[JTreeSet] {
    protected def instantiate[T](ord: Ordering[T]): JTreeSet[T] = new JTreeSet[T](ord)
  }

  object JEnumSet {
    def allOf[T <: Enum[T]: ClassTag]: JEnumSet[T] =
      ju.EnumSet.allOf(classTag[T].runtimeClass.asInstanceOf[Class[T]])

    def empty[T <: Enum[T]: ClassTag]: JEnumSet[T] =
      ju.EnumSet.noneOf(classTag[T].runtimeClass.asInstanceOf[Class[T]])

    def apply[T <: Enum[T]: ClassTag](values: T*): JEnumSet[T] = {
      val result = empty[T]
      values.foreach(result.add)
      result
    }
  }

  abstract class JMapCreator[M[K, V] <: JMap[K, V]] {
    protected def instantiate[K, V]: M[K, V]

    def empty[K, V]: M[K, V] = instantiate[K, V]

    def apply[K, V](entries: (K, V)*): M[K, V] = {
      val result = instantiate[K, V]
      entries.foreach { case (k, v) => result.put(k, v) }
      result
    }
  }
  object JMapCreator {
    implicit def asJMapFactory[M[X, Y] <: JMap[X, Y], K, V](creator: JMapCreator[M]): Factory[(K, V), M[K, V]] =
      new JMapFactory(creator.empty[K, V])
  }

  abstract class JSortedMapCreator[M[K, V] <: JSortedMap[K, V]] {
    protected def instantiate[K: Ordering, V]: M[K, V]

    def empty[K: Ordering, V]: M[K, V] = instantiate[K, V]

    def apply[K: Ordering, V](entries: (K, V)*): M[K, V] = {
      val result = instantiate[K, V]
      entries.foreach { case (k, v) => result.put(k, v) }
      result
    }

    def unapplySeq[K, V](map: M[K, V]): Option[Seq[(K, V)]] =
      Some(map.asScala.iterator.toSeq)
  }
  object JSortedMapCreator {
    implicit def asJSortedMapFactory[M[X, Y] <: JSortedMap[X, Y], K: Ordering, V](
      creator: JSortedMapCreator[M],
    ): Factory[(K, V), M[K, V]] =
      new JMapFactory(creator.empty[K, V])
  }

  object JMap extends JMapCreator[JMap] {
    protected def instantiate[K, V]: JMap[K, V] = new JHashMap[K, V]
  }

  object JHashMap extends JMapCreator[JHashMap] {
    protected def instantiate[K, V]: JHashMap[K, V] = new JHashMap[K, V]
  }

  object JLinkedHashMap extends JMapCreator[JLinkedHashMap] {
    protected def instantiate[K, V]: JLinkedHashMap[K, V] = new JLinkedHashMap[K, V]

    def unapplySeq[K, V](map: JLinkedHashMap[K, V]): Option[Seq[(K, V)]] =
      Some(map.asScala.iterator.toSeq)
  }

  object JSortedMap extends JSortedMapCreator[JSortedMap] {
    protected def instantiate[K: Ordering, V]: JSortedMap[K, V] = new JTreeMap[K, V](Ordering[K])
  }

  object JNavigableMap extends JSortedMapCreator[JNavigableMap] {
    protected def instantiate[K: Ordering, V]: JNavigableMap[K, V] = new JTreeMap[K, V](Ordering[K])
  }

  object JTreeMap extends JSortedMapCreator[JTreeMap] {
    protected def instantiate[K: Ordering, V]: JTreeMap[K, V] = new JTreeMap[K, V](Ordering[K])
  }

  object JEnumMap {
    def empty[K <: Enum[K]: ClassTag, V]: JEnumMap[K, V] =
      new JEnumMap[K, V](classTag[K].runtimeClass.asInstanceOf[Class[K]])

    def apply[K <: Enum[K]: ClassTag, V](keyValues: (K, V)*): JEnumMap[K, V] = {
      val result = empty[K, V]
      keyValues.foreach { case (k, v) =>
        result.put(k, v)
      }
      result
    }
  }

  import JCollectionUtils.*

  implicit def pairIterableOps[A, B](coll: IterableOnce[(A, B)]): pairIterableOps[A, B] = new pairIterableOps(coll)
}

object JCollectionUtils {
  class pairIterableOps[A, B](private val coll: IterableOnce[(A, B)]) extends AnyVal {
    def toJMap[M[K, V] <: JMap[K, V]](implicit fac: Factory[(A, B), M[A, B]]): M[A, B] = {
      val b = fac.newBuilder
      coll.iterator.foreach(b += _)
      b.result()
    }
  }
}
