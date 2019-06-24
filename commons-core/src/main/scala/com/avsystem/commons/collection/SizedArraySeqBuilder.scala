package com.avsystem.commons
package collection

import scala.collection.compat._

final class SizedArraySeqBuilder[A](expectedSize: Int) extends CrossBuilder[A, IArraySeq[A]] {
  private[this] val array = new Array[AnyRef](expectedSize).asInstanceOf[Array[A]]
  private[this] var idx: Int = 0

  def size: Int = idx

  def clear(): Unit = {
    idx = 0
  }

  def result(): IArraySeq[A] = {
    if (idx < expectedSize) {
      throw new IllegalStateException(s"exactly $expectedSize elements were expected but only $idx were added")
    }
    IArraySeq.unsafeWrapArray(array)
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

final class SizedArraySeqFactory[A](size: Int) extends CrossFactory[A, IArraySeq[A]] {
  def fromSpecific(it: IterableOnce[A]): IArraySeq[A] =
    IArraySeq.unsafeWrapArray(it.iterator.toArray[Any].asInstanceOf[Array[A]])

  def newBuilder: MBuilder[A, IArraySeq[A]] =
    new SizedArraySeqBuilder[A](size)
}
