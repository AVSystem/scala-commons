package com.avsystem.commons
package collection

import scala.collection.{immutable as sci, mutable as scm}
import scala.collection as sc

/** Aliases for Scala collections which are both concise and leave no doubt about whether the collection type is
  * immutable, mutable or the base type (read only).
  *
  * `B` stands for base (read only) `I` stands for immutable `M` stands for mutable
  */
trait CollectionAliases {
  export sc.Iterable as BIterable
  export sci.Iterable as IIterable
  export scm.Iterable as MIterable

  export sc.Seq as BSeq
  export sci.Seq as ISeq
  export scm.Seq as MSeq

  export sc.IndexedSeq as BIndexedSeq
  export sci.IndexedSeq as IIndexedSeq
  export scm.IndexedSeq as MIndexedSeq

  export scm.ArraySeq as MArraySeq
  export sci.ArraySeq as IArraySeq

  export sc.LinearSeq as BLinearSeq
  export sci.LinearSeq as ILinearSeq

  export sci.Queue as IQueue
  export scm.Queue as MQueue

  export sc.Set as BSet
  export sci.Set as ISet
  export scm.Set as MSet

  export sci.HashSet as IHashSet
  export scm.HashSet as MHashSet

  export sc.SortedSet as BSortedSet
  export sci.SortedSet as ISortedSet
  export scm.SortedSet as MSortedSet

  export sci.TreeSet as ITreeSet
  export scm.TreeSet as MTreeSet

  export sc.BitSet as BBitSet
  export sci.BitSet as IBitSet
  export scm.BitSet as MBitSet

  export scm.LinkedHashSet as MLinkedHashSet

  export sc.Map as BMap
  export sci.Map as IMap
  export scm.Map as MMap

  export sci.HashMap as IHashMap
  export scm.HashMap as MHashMap

  export scm.LinkedHashMap as MLinkedHashMap

  export sci.ListMap as IListMap

  export sc.SortedMap as BSortedMap
  export sci.SortedMap as ISortedMap

  export sci.TreeMap as ITreeMap

  export scm.TreeMap as MTreeMap

  export scm.Buffer as MBuffer

  export scm.Builder as MBuilder
  type MColBuilder[Elem, +Col[_]] = scm.Builder[Elem, Col[Elem]]

  export scm.ListBuffer as MListBuffer

  export scm.ArrayBuffer as MArrayBuffer
}

object CollectionAliases extends CollectionAliases
