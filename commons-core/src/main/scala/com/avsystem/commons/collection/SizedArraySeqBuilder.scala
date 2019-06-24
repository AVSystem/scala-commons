package com.avsystem.commons
package collection

import scala.collection.compat._
import scala.collection.compat.immutable.ArraySeq

final class SizedArraySeqBuilder[A](expectedSize: Int) extends CrossBuilder[A, ArraySeq[A]] {
  private[this] val array = new Array[AnyRef](expectedSize).asInstanceOf[Array[A]]
  private[this] var idx: Int = 0

  def size: Int = idx

  def clear(): Unit = {
    idx = 0
  }

  def result(): ArraySeq[A] = {
    if (idx < expectedSize) {
      throw new IllegalStateException(s"exactly $expectedSize elements were expected but only $idx were added")
    }
    ArraySeq.unsafeWrapArray(array)
  }

  def addOne(elem: A): this.type = {
    array(idx) = elem
    idx += 1
    this
  }

  override def sizeHint(sh: Int): Unit =
    if (sh != expectedSize) {
      throw new IllegalArgumentException(s"required exactly $expectedSize elements but got size hint $sh")
    }
}

final class SizedArraySeqFactory[A](size: Int) extends CrossFactory[A, ArraySeq[A]] {
  def fromSpecific(it: IterableOnce[A]): ArraySeq[A] =
    ArraySeq.unsafeWrapArray(it.iterator.toArray[Any].asInstanceOf[Array[A]])

  def newBuilder: MBuilder[A, ArraySeq[A]] =
    new SizedArraySeqBuilder[A](size)
}
