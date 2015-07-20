package com.avsystem.commons
package jiop

import java.{lang => jl, util => ju}

import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

trait JCollectionUtils extends JIterableCBF {
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

  trait JCollectionCreator[C[T] <: JCollection[T]] {
    protected def instantiate[T]: C[T]

    def apply[T](values: T*): C[T] = {
      val result = instantiate[T]
      result.addAll(values.asJava)
      result
    }
  }

  object JList {
    def unapplySeq[T](list: JList[T]): Option[Seq[T]] =
      Some(list.asScala)
  }

  object JArrayList extends JCollectionCreator[JArrayList] {
    protected def instantiate[T]: JArrayList[T] = new JArrayList[T]

    def unapplySeq[T](list: JArrayList[T]): Option[Seq[T]] =
      Some(list.asScala)
  }

  object JLinkedList extends JCollectionCreator[JLinkedList] {
    protected def instantiate[T]: JLinkedList[T] = new JLinkedList[T]

    def unapplySeq[T](list: JLinkedList[T]): Option[Seq[T]] =
      Some(list.asScala)
  }

  object JHashSet extends JCollectionCreator[JHashSet] {
    protected def instantiate[T]: JHashSet[T] = new JHashSet[T]
  }

  object JLinkedHashSet extends JCollectionCreator[JLinkedHashSet] {
    protected def instantiate[T]: JLinkedHashSet[T] = new JLinkedHashSet[T]

    def unapplySeq[T](set: JLinkedHashSet[T]): Option[Seq[T]] =
      Some(set.iterator.asScala.toStream)
  }

  object JSortedSet {
    def unapplySeq[T](set: JSortedSet[T]): Option[Seq[T]] =
      Some(set.iterator.asScala.toStream)
  }

  object JNavigableSet {
    def unapplySeq[T](set: JNavigableSet[T]): Option[Seq[T]] =
      Some(set.iterator.asScala.toStream)
  }

  object JTreeSet {
    def apply[T: Ordering](values: T*): JTreeSet[T] = {
      val result = new JTreeSet[T](Ordering[T])
      result.addAll(values.asJava)
      result
    }

    def unapplySeq[T](set: JTreeSet[T]): Option[Seq[T]] =
      Some(set.iterator.asScala.toStream)
  }

  object JHashMap {
    def apply[K, V](entries: (K, V)*): JHashMap[K, V] = {
      val result = new JHashMap[K, V]
      entries.foreach { case (k, v) => result.put(k, v) }
      result
    }
  }

  object JLinkedHashMap {
    def apply[K, V](entries: (K, V)*): JLinkedHashMap[K, V] = {
      val result = new JLinkedHashMap[K, V]
      entries.foreach { case (k, v) => result.put(k, v) }
      result
    }

    def unapplySeq[K, V](map: JLinkedHashMap[K, V]): Option[Seq[(K, V)]] =
      Some(map.asScala.iterator.toStream)
  }

  object JSortedMap {
    def unapplySeq[K, V](map: JSortedMap[K, V]): Option[Seq[(K, V)]] =
      Some(map.asScala.iterator.toStream)
  }

  object JNavigableMap {
    def unapplySeq[K, V](map: JNavigableMap[K, V]): Option[Seq[(K, V)]] =
      Some(map.asScala.iterator.toStream)
  }

  object JTreeMap {
    def apply[K: Ordering, V](entries: (K, V)*): JTreeMap[K, V] = {
      val result = new JTreeMap[K, V](Ordering[K])
      entries.foreach { case (k, v) => result.put(k, v) }
      result
    }

    def unapplySeq[K, V](map: JTreeMap[K, V]): Option[Seq[(K, V)]] =
      Some(map.asScala.iterator.toStream)
  }

  import JCollectionUtils._

  implicit def iteratorOps[A](it: JIterator[A]): iteratorOps[A] = new iteratorOps(it)
  implicit def iterableOps[A](it: JIterable[A]): iterableOps[A] = new iterableOps(it)
  implicit def collectionOps[A](it: JCollection[A]): collectionOps[A] = new collectionOps(it)
  implicit def intCollectionOps(it: JCollection[Int]): intCollectionOps = new intCollectionOps(it)
  implicit def longCollectionOps(it: JCollection[Long]): longCollectionOps = new longCollectionOps(it)
  implicit def doubleCollectionOps(it: JCollection[Double]): doubleCollectionOps = new doubleCollectionOps(it)
}

trait JIterableCBF extends JSetCBF with JLinkedListCBF {
  this: JCollectionUtils =>
  // for JIterable, JCollection, JList and JArrayList
  implicit def jArrayListCBF[A]: CanBuildFrom[Nothing, A, JArrayList[A]] =
    new JCollectionUtils.JCollectionCBF(new JArrayList)
}

trait JSetCBF extends JSortedSetCBF {
  this: JCollectionUtils =>
  // for JSet and JHashSet
  implicit def jHashSetCBF[A]: CanBuildFrom[Nothing, A, JHashSet[A]] =
    new JCollectionUtils.JCollectionCBF(new JHashSet)

  implicit def jLinkedHashSetCBF[A]: CanBuildFrom[Nothing, A, JLinkedHashSet[A]] =
    new JCollectionUtils.JCollectionCBF(new JLinkedHashSet)
}

trait JSortedSetCBF {
  this: JCollectionUtils =>
  // for JSortedSet, JNavigableSet and JTreeSet
  implicit def jTreeSetCBF[A]: CanBuildFrom[Nothing, A, JTreeSet[A]] =
    new JCollectionUtils.JCollectionCBF(new JTreeSet)
}

trait JLinkedListCBF {
  this: JCollectionUtils =>
  implicit def jLinkedListCBF[A]: CanBuildFrom[Nothing, A, JLinkedList[A]] =
    new JCollectionUtils.JCollectionCBF(new JLinkedList)
}

object JCollectionUtils {
  import JavaInterop._

  private[jiop] class JCollectionCBF[A, C <: JCollection[A]](creator: => C) extends CanBuildFrom[Nothing, A, C] {
    def apply(from: Nothing) = apply()
    def apply() = new mutable.Builder[A, C] {
      val coll = creator

      def +=(elem: A): this.type = {
        coll.add(elem)
        this
      }

      def clear(): Unit =
        coll.clear()

      def result(): C =
        coll
    }
  }

  class iteratorOps[A](private val it: JIterator[A]) extends AnyVal {
    def forEachRemaining(code: A => Any): Unit =
      it.forEachRemaining(jConsumer(code))
  }

  class iterableOps[A](private val it: JIterable[A]) extends AnyVal {
    def forEach(code: A => Any): Unit =
      it.forEach(jConsumer(code))
  }

  class collectionOps[A](private val coll: JCollection[A]) extends AnyVal {
    def removeIf(pred: A => Boolean): Unit =
      coll.removeIf(jPredicate(pred))

    def scalaStream: ScalaJStream[A] =
      coll.stream.asScala
  }

  class intCollectionOps(private val coll: JCollection[Int]) extends AnyVal {
    def scalaIntStream: ScalaJIntStream =
      coll.stream.asScalaIntStream
  }

  class longCollectionOps(private val coll: JCollection[Long]) extends AnyVal {
    def scalaLongStream: ScalaJLongStream =
      coll.stream.asScalaLongStream
  }

  class doubleCollectionOps(private val coll: JCollection[Double]) extends AnyVal {
    def scalaDoubleStream: ScalaJDoubleStream =
      coll.stream.asScalaDoubleStream
  }
}
