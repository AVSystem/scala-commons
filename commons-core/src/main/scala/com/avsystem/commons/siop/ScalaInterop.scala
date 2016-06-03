package com.avsystem.commons
package siop

import com.avsystem.commons.collection.CollectionAliases.{BBitSet, BIterable, BMap, BSeq, BSet, IBitSet, IIterable, IMap, ISeq, ISet, MBitSet, MIterable, MMap, MSeq, MSet}
import com.avsystem.commons.jiop.JavaInterop._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Allows to convert scala collections to and from java collections from Java code
  */
object ScalaInterop {

  def asJava[T](iterable: BIterable[T]): JIterable[T] = iterable.asJava
  def asJava[T](iterable: IIterable[T]): JIterable[T] = iterable.asJava
  def asJava[T](iterable: MIterable[T]): JIterable[T] = iterable.asJava

  def asJava[T](seq: BSeq[T]): JList[T] = seq.asJava
  def asJava[T](seq: ISeq[T]): JList[T] = seq.asJava
  def asJava[T](seq: MSeq[T]): JList[T] = seq.asJava

  def asJava[T](set: BSet[T]): JSet[T] = set.asJava
  def asJava[T](set: ISet[T]): JSet[T] = set.asJava
  def asJava[T](set: MSet[T]): JSet[T] = set.asJava

  def asJava[T](set: BBitSet): JSet[Int] = set.asJava
  def asJava[T](set: IBitSet): JSet[Int] = set.asJava
  def asJava[T](set: MBitSet): JSet[Int] = set.asJava

  def asJava[K, V](map: BMap[K, V]): JMap[K, V] = map.asJava
  def asJava[K, V](map: IMap[K, V]): JMap[K, V] = map.asJava
  def asJava[K, V](map: MMap[K, V]): JMap[K, V] = map.asJava

  def asJava[T](iterator: Iterator[T]): JIterator[T] = iterator.asJava

  def asJava[T](list: List[T]): JList[T] = list.asJava

  def asJava[T](listBuffer: ListBuffer[T]) = listBuffer.asJava

  def asJava[T](option: Option[T]): JOptional[T] = option.asJava



  def asScala[T](collection: JCollection[T]): BIterable[T] = collection.asScala

  def asScala[T](iterable: JIterable[T]): BIterable[T] = iterable.asScala

  def asScala[T](seq: JList[T]): mutable.Buffer[T] = seq.asScala

  def asScala[T](set: JSet[T]): MSet[T] = set.asScala

  def asScala[K, V](map: JMap[K, V]): MMap[K, V] = map.asScala


  def asScala[T](iterator: JIterator[T]): Iterator[T] = iterator.asScala

  def asScala[T](option: JOptional[T]): Option[T] = option.asScala


  def asScalaImmutable[T](iterable: JIterable[T]): IIterable[T] = iterable.asScala.toList

  def asScalaImmutable[T](seq: JList[T]): List[T] = seq.asScala.toList

  def asScalaImmutable[T](set: JSet[T]): ISet[T] = set.asScala.toSet

  def asScalaImmutable[K, V](map: JMap[K, V]): IMap[K, V] = map.asScala.toMap

}
