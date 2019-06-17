package com.avsystem.commons
package collection

import scala.collection.immutable.ArraySeq
import scala.collection.{Factory, mutable}

final class UnsafeArraySeqBuilder[A](expectedSize: Int) extends mutable.Builder[A, ArraySeq[A]] {
  private[this] var array = new Array[AnyRef](expectedSize).asInstanceOf[Array[A]]
  private[this] var idx: Int = 0

  def size: Int = idx

  def clear(): Unit = {
    array = new Array[AnyRef](expectedSize).asInstanceOf[Array[A]]
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

final class UnsafeArraySeqFactory[A](size: Int) extends Factory[A, ArraySeq[A]] {
  def fromSpecific(it: IterableOnce[A]): ArraySeq[A] =
    it.knownSize match {
      case -1 =>
        val b = new MArrayBuffer[A]
        b.addAll(it)
        ArraySeq.unsafeWrapArray(b.toArray[Any].asInstanceOf[Array[A]])
      case size =>
        val b = new UnsafeArraySeqBuilder[A](size)
        b.addAll(it)
        b.result()
    }

  def newBuilder: MBuilder[A, ArraySeq[A]] =
    new UnsafeArraySeqBuilder[A](size)
}
