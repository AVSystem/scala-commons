package com.avsystem.commons
package redis.util

import com.avsystem.commons.collection.{CrossBuilder, CrossFactory}

final class SizedArraySeqBuilder[A](val expectedSize: Int) extends CrossBuilder[A, IArraySeq[A]] {
  private[this] val array = new Array[AnyRef](expectedSize).asInstanceOf[Array[A]]
  private[this] var idx: Int = 0

  def complete: Boolean =
    idx >= expectedSize

  def clear(): Unit = {
    idx = 0
  }

  def result(): IArraySeq[A] = {
    if (!complete) {
      throw new IllegalStateException(s"exactly $expectedSize elements were expected but only $idx were added")
    }
    IArraySeq.unsafeWrapArray(array)
  }

  def addOne(elem: A): this.type = {
    if (complete) {
      throw new IllegalStateException(s"exactly $expectedSize elements were expected but more were added")
    }
    array(idx) = elem
    idx += 1
    this
  }

  override def sizeHint(sh: Int): Unit =
    if (sh != expectedSize) {
      throw new IllegalArgumentException(s"required exactly $expectedSize elements but got size hint $sh")
    }
}

final class SizedArraySeqFactory[A](size: Int) extends CrossFactory[A, IArraySeq[A]] {
  def fromSpecific(it: IterableOnce[A]): IArraySeq[A] =
    IArraySeq.unsafeWrapArray(it.iterator.toArray[Any].asInstanceOf[Array[A]])

  def newBuilder: MBuilder[A, IArraySeq[A]] =
    new SizedArraySeqBuilder[A](size)
}
