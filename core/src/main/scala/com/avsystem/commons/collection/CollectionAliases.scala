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
  type BIterable[+A] = sc.Iterable[A]
  final def BIterable: sc.Iterable.type = sc.Iterable
  type IIterable[+A] = sci.Iterable[A]
  final def IIterable: sci.Iterable.type = sci.Iterable
  type MIterable[A] = scm.Iterable[A]
  final def MIterable: scm.Iterable.type = scm.Iterable

  type BSeq[+A] = sc.Seq[A]
  final def BSeq: sc.Seq.type = sc.Seq
  type ISeq[+A] = sci.Seq[A]
  final def ISeq: sci.Seq.type = sci.Seq
  type MSeq[A] = scm.Seq[A]
  final def MSeq: scm.Seq.type = scm.Seq

  type BIndexedSeq[+A] = sc.IndexedSeq[A]
  final def BIndexedSeq: sc.IndexedSeq.type = sc.IndexedSeq
  type IIndexedSeq[+A] = sci.IndexedSeq[A]
  final def IIndexedSeq: sci.IndexedSeq.type = sci.IndexedSeq
  type MIndexedSeq[A] = scm.IndexedSeq[A]
  final def MIndexedSeq: scm.IndexedSeq.type = scm.IndexedSeq

  type MArraySeq[A] = scm.ArraySeq[A]
  final def MArraySeq: scm.ArraySeq.type = scm.ArraySeq
  type IArraySeq[+A] = sci.ArraySeq[A]
  final def IArraySeq: sci.ArraySeq.type = sci.ArraySeq

  type BLinearSeq[+A] = sc.LinearSeq[A]
  final def BLinearSeq: sc.LinearSeq.type = sc.LinearSeq
  type ILinearSeq[+A] = sci.LinearSeq[A]
  final def ILinearSeq: sci.LinearSeq.type = sci.LinearSeq

  type IQueue[+A] = sci.Queue[A]
  final def IQueue: sci.Queue.type = sci.Queue
  type MQueue[A] = scm.Queue[A]
  final def MQueue: scm.Queue.type = scm.Queue

  type BSet[A] = sc.Set[A]
  final def BSet: sc.Set.type = sc.Set
  type ISet[A] = sci.Set[A]
  final def ISet: Set.type = sci.Set
  type MSet[A] = scm.Set[A]
  final def MSet: scm.Set.type = scm.Set

  type IHashSet[A] = sci.HashSet[A]
  final def IHashSet: sci.HashSet.type = sci.HashSet
  type MHashSet[A] = scm.HashSet[A]
  final def MHashSet: scm.HashSet.type = scm.HashSet

  type BSortedSet[A] = sc.SortedSet[A]
  final def BSortedSet: sc.SortedSet.type = sc.SortedSet
  type ISortedSet[A] = sci.SortedSet[A]
  final def ISortedSet: sci.SortedSet.type = sci.SortedSet
  type MSortedSet[A] = scm.SortedSet[A]
  final def MSortedSet: scm.SortedSet.type = scm.SortedSet

  type ITreeSet[A] = sci.TreeSet[A]
  final def ITreeSet: sci.TreeSet.type = sci.TreeSet
  type MTreeSet[A] = scm.TreeSet[A]
  final def MTreeSet: scm.TreeSet.type = scm.TreeSet

  type BBitSet = sc.BitSet
  final def BBitSet: sc.BitSet.type = sc.BitSet
  type IBitSet = sci.BitSet
  final def IBitSet: sci.BitSet.type = sci.BitSet
  type MBitSet = scm.BitSet
  final def MBitSet: scm.BitSet.type = scm.BitSet

  type MLinkedHashSet[A] = scm.LinkedHashSet[A]
  final def MLinkedHashSet: scm.LinkedHashSet.type = scm.LinkedHashSet

  type BMap[A, +B] = sc.Map[A, B]
  final def BMap: sc.Map.type = sc.Map
  type IMap[A, +B] = sci.Map[A, B]
  final def IMap: sci.Map.type = sci.Map
  type MMap[A, B] = scm.Map[A, B]
  final def MMap: scm.Map.type = scm.Map

  type IHashMap[A, +B] = sci.HashMap[A, B]
  final def IHashMap: sci.HashMap.type = sci.HashMap
  type MHashMap[A, B] = scm.HashMap[A, B]
  final def MHashMap: scm.HashMap.type = scm.HashMap

  type MLinkedHashMap[A, B] = scm.LinkedHashMap[A, B]
  final def MLinkedHashMap: scm.LinkedHashMap.type = scm.LinkedHashMap

  type IListMap[A, +B] = sci.ListMap[A, B]
  final def IListMap: sci.ListMap.type = sci.ListMap

  type BSortedMap[A, +B] = sc.SortedMap[A, B]
  final def BSortedMap: sc.SortedMap.type = sc.SortedMap
  type ISortedMap[A, +B] = sci.SortedMap[A, B]
  final def ISortedMap: sci.SortedMap.type = sci.SortedMap

  type ITreeMap[A, +B] = sci.TreeMap[A, B]
  final def ITreeMap: sci.TreeMap.type = sci.TreeMap

  type MTreeMap[A, B] = scm.TreeMap[A, B]
  final def MTreeMap: scm.TreeMap.type = scm.TreeMap

  type MBuffer[A] = scm.Buffer[A]
  final def MBuffer: scm.Buffer.type = scm.Buffer

  type MBuilder[-Elem, +To] = scm.Builder[Elem, To]
  type MColBuilder[Elem, +Col[_]] = scm.Builder[Elem, Col[Elem]]

  type MListBuffer[A] = scm.ListBuffer[A]
  final def MListBuffer: scm.ListBuffer.type = scm.ListBuffer

  type MArrayBuffer[A] = scm.ArrayBuffer[A]
  final def MArrayBuffer: scm.ArrayBuffer.type = scm.ArrayBuffer
}

object CollectionAliases extends CollectionAliases
