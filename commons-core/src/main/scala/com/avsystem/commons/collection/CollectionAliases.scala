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
object CollectionAliases {
  type BTraversable[+A] = sc.Traversable[A]
  val BTraversable = sc.Traversable
  type ITraversable[+A] = sci.Traversable[A]
  val ITraversable = sci.Traversable
  type MTraversable[A] = scm.Traversable[A]
  val MTraversable = scm.Traversable

  type BIterable[+A] = sc.Iterable[A]
  val BIterable = sc.Iterable
  type IIterable[+A] = sci.Iterable[A]
  val IIterable = sci.Iterable
  type MIterable[A] = scm.Iterable[A]
  val MIterable = scm.Iterable

  type BSeq[+A] = sc.Seq[A]
  val BSeq = sc.Seq
  type ISeq[+A] = sci.Seq[A]
  val ISeq = sci.Seq
  type MSeq[A] = scm.Seq[A]
  val MSeq = scm.Seq

  type BIndexedSeq[+A] = sc.IndexedSeq[A]
  val BIndexedSeq = sc.IndexedSeq
  type IIndexedSeq[+A] = sci.IndexedSeq[A]
  val IIndexedSeq = sci.IndexedSeq
  type MIndexedSeq[A] = scm.IndexedSeq[A]
  val MIndexedSeq = scm.IndexedSeq

  type BLinearSeq[+A] = sc.LinearSeq[A]
  val BLinearSeq = sc.LinearSeq
  type ILinearSeq[+A] = sci.LinearSeq[A]
  val ILinearSeq = sci.LinearSeq
  type MLinearSeq[A] = scm.LinearSeq[A]
  val MLinearSeq = scm.LinearSeq

  type BSet[A] = sc.Set[A]
  val BSet = sc.Set
  type ISet[A] = sci.Set[A]
  val ISet = sci.Set
  type MSet[A] = scm.Set[A]
  val MSet = scm.Set

  type IHashSet[A] = sci.HashSet[A]
  val IHashSet = sci.HashSet
  type MHashSet[A] = scm.HashSet[A]
  val MHashSet = scm.HashSet

  type BSortedSet[A] = sc.SortedSet[A]
  val BSortedSet = sc.SortedSet
  type ISortedSet[A] = sci.SortedSet[A]
  val ISortedSet = sci.SortedSet
  type MSortedSet[A] = scm.SortedSet[A]
  val MSortedSet = scm.SortedSet

  type ITreeSet[A] = sci.TreeSet[A]
  val ITreeSet = sci.TreeSet
  type MTreeSet[A] = scm.TreeSet[A]
  val MTreeSet = scm.TreeSet

  type BBitSet = sc.BitSet
  val BBitSet = sc.BitSet
  type IBitSet = sci.BitSet
  val IBitSet = sci.BitSet
  type MBitSet = scm.BitSet
  val MBitSet = scm.BitSet

  type BMap[A, +B] = sc.Map[A, B]
  val BMap = sc.Map
  type IMap[A, +B] = sci.Map[A, B]
  val IMap = sci.Map
  type MMap[A, B] = scm.Map[A, B]
  val MMap = scm.Map

  type IHashMap[A, +B] = sci.HashMap[A, B]
  val IHashMap = sci.HashMap
  type MHashMap[A, B] = scm.HashMap[A, B]
  val MHashMap = scm.HashMap

  type IListMap[A, +B] = sci.ListMap[A, B]
  val IListMap = sci.ListMap
  type MListMap[A, B] = scm.ListMap[A, B]
  val MListMap = scm.ListMap

  type BSortedMap[A, +B] = sc.SortedMap[A, B]
  val BSortedMap = sc.SortedMap
  type ISortedMap[A, +B] = sci.SortedMap[A, B]
  val ISortedMap = sci.SortedMap

  type ITreeMap[A, +B] = sci.TreeMap[A, B]
  val ITreeMap = sci.TreeMap
  // Coming in Scala 2.12
  // type MTreeMap[A, B] = scm.TreeMap[A, B]
  // val MTreeMap = scm.TreeMap
}
