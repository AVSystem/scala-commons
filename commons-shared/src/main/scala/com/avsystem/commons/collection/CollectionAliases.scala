package com.avsystem.commons
package collection

import scala.collection.{immutable => sci, mutable => scm}
import scala.{collection => sc}

/**
  * Aliases for Scala collections which are both concise and leave no doubt about whether the collection type is
  * immutable, mutable or the base type (read only).
  *
  * `B` stands for base (read only)
  * `I` stands for immutable
  * `M` stands for mutable
  */
trait CollectionAliases {
  type BTraversable[+A] = sc.Traversable[A]
  final val BTraversable = sc.Traversable
  type ITraversable[+A] = sci.Traversable[A]
  final val ITraversable = sci.Traversable
  type MTraversable[A] = scm.Traversable[A]
  final val MTraversable = scm.Traversable

  type BIterable[+A] = sc.Iterable[A]
  final val BIterable = sc.Iterable
  type IIterable[+A] = sci.Iterable[A]
  final val IIterable = sci.Iterable
  type MIterable[A] = scm.Iterable[A]
  final val MIterable = scm.Iterable

  type BSeq[+A] = sc.Seq[A]
  final val BSeq = sc.Seq
  type ISeq[+A] = sci.Seq[A]
  final val ISeq = sci.Seq
  type MSeq[A] = scm.Seq[A]
  final val MSeq = scm.Seq

  type BIndexedSeq[+A] = sc.IndexedSeq[A]
  final val BIndexedSeq = sc.IndexedSeq
  type IIndexedSeq[+A] = sci.IndexedSeq[A]
  final val IIndexedSeq = sci.IndexedSeq
  type MIndexedSeq[A] = scm.IndexedSeq[A]
  final val MIndexedSeq = scm.IndexedSeq

  type BLinearSeq[+A] = sc.LinearSeq[A]
  final val BLinearSeq = sc.LinearSeq
  type ILinearSeq[+A] = sci.LinearSeq[A]
  final val ILinearSeq = sci.LinearSeq
  type MLinearSeq[A] = scm.LinearSeq[A]
  final val MLinearSeq = scm.LinearSeq

  type IQueue[+A] = sci.Queue[A]
  final val IQueue = sci.Queue
  type MQueue[A] = scm.Queue[A]
  final val MQueue = scm.Queue

  type BSet[A] = sc.Set[A]
  final val BSet = sc.Set
  type ISet[A] = sci.Set[A]
  final val ISet = sci.Set
  type MSet[A] = scm.Set[A]
  final val MSet = scm.Set

  type IHashSet[A] = sci.HashSet[A]
  final val IHashSet = sci.HashSet
  type MHashSet[A] = scm.HashSet[A]
  final val MHashSet = scm.HashSet

  type BSortedSet[A] = sc.SortedSet[A]
  final val BSortedSet = sc.SortedSet
  type ISortedSet[A] = sci.SortedSet[A]
  final val ISortedSet = sci.SortedSet
  type MSortedSet[A] = scm.SortedSet[A]
  final val MSortedSet = scm.SortedSet

  type ITreeSet[A] = sci.TreeSet[A]
  final val ITreeSet = sci.TreeSet
  type MTreeSet[A] = scm.TreeSet[A]
  final val MTreeSet = scm.TreeSet

  type BBitSet = sc.BitSet
  final val BBitSet = sc.BitSet
  type IBitSet = sci.BitSet
  final val IBitSet = sci.BitSet
  type MBitSet = scm.BitSet
  final val MBitSet = scm.BitSet

  type BMap[A, +B] = sc.Map[A, B]
  final val BMap = sc.Map
  type IMap[A, +B] = sci.Map[A, B]
  final val IMap = sci.Map
  type MMap[A, B] = scm.Map[A, B]
  final val MMap = scm.Map

  type IHashMap[A, +B] = sci.HashMap[A, B]
  final val IHashMap = sci.HashMap
  type MHashMap[A, B] = scm.HashMap[A, B]
  final val MHashMap = scm.HashMap

  type IListMap[A, +B] = sci.ListMap[A, B]
  final val IListMap = sci.ListMap
  type MListMap[A, B] = scm.ListMap[A, B]
  final val MListMap = scm.ListMap

  type BSortedMap[A, +B] = sc.SortedMap[A, B]
  final val BSortedMap = sc.SortedMap
  type ISortedMap[A, +B] = sci.SortedMap[A, B]
  final val ISortedMap = sci.SortedMap

  type ITreeMap[A, +B] = sci.TreeMap[A, B]
  final val ITreeMap = sci.TreeMap
  // Coming in Scala 2.12
  // type MTreeMap[A, B] = scm.TreeMap[A, B]
  // final val MTreeMap = scm.TreeMap
}

object CollectionAliases extends CollectionAliases
