package com.avsystem.commons
package jiop

import java.{lang => jl, util => ju}

import com.avsystem.commons.jiop.BasicJavaInterop._

import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}

trait JCollectionUtils extends JCanBuildFroms {
  type JIterator[T] = ju.Iterator[T]
  type JIterable[T] = jl.Iterable[T]
  type JCollection[T] = ju.Collection[T]
  type JList[T] = ju.List[T]
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

    def empty[T]: C[T] = instantiate[T]

    def apply[T](values: T*): C[T] = {
      val result = instantiate[T]
      result.addAll(values.asJava)
      result
    }
  }

  abstract class JListCreator[C[T] <: JList[T]] extends JCollectionCreator[C] {
    def unapplySeq[T](list: C[T]): Option[Seq[T]] =
      Some(list.asScala)
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
      Some(set.iterator.asScala.toStream)
  }

  object JIterable {
    def apply[T](values: T*): JIterable[T] =
      JArrayList(values: _*)
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
      Some(set.iterator.asScala.toStream)
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

  abstract class JMapCreator[M[K, V] <: JMap[K, V]] {
    protected def instantiate[K, V]: M[K, V]

    def empty[K, V]: M[K, V] = instantiate[K, V]

    def apply[K, V](entries: (K, V)*): M[K, V] = {
      val result = instantiate[K, V]
      entries.foreach { case (k, v) => result.put(k, v) }
      result
    }
  }

  abstract class JSortedMapCreator[M[K, V] <: JSortedMap[K, V]] {
    protected def instantiate[K, V](ord: Ordering[K]): M[K, V]

    def apply[K: Ordering, V](entries: (K, V)*): M[K, V] = {
      val result = instantiate[K, V](Ordering[K])
      entries.foreach { case (k, v) => result.put(k, v) }
      result
    }

    def unapplySeq[K, V](map: M[K, V]): Option[Seq[(K, V)]] =
      Some(map.asScala.iterator.toStream)
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
      Some(map.asScala.iterator.toStream)
  }

  object JSortedMap extends JSortedMapCreator[JSortedMap] {
    protected def instantiate[K, V](ord: Ordering[K]): JSortedMap[K, V] = new JTreeMap[K, V](ord)
  }

  object JNavigableMap extends JSortedMapCreator[JNavigableMap] {
    protected def instantiate[K, V](ord: Ordering[K]): JNavigableMap[K, V] = new JTreeMap[K, V](ord)
  }

  object JTreeMap extends JSortedMapCreator[JTreeMap] {
    protected def instantiate[K, V](ord: Ordering[K]): JTreeMap[K, V] = new JTreeMap[K, V](ord)
  }

  import JCollectionUtils._

  implicit def pairTraversableOps[A, B](coll: TraversableOnce[(A, B)]): pairTraversableOps[A, B] = new pairTraversableOps(coll)
}

object JCollectionUtils {
  class pairTraversableOps[A, B](private val coll: TraversableOnce[(A, B)]) extends AnyVal {
    def toJMap[M[K, V] <: JMap[K, V]](implicit cbf: CanBuildFrom[Nothing, (A, B), M[A, B]]): M[A, B] = {
      val b = cbf()
      coll.foreach(b += _)
      b.result()
    }
  }
}
