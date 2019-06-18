package com.avsystem.commons
package collection

import scala.collection.compat._
import scala.collection.compat.immutable.ArraySeq

trait CrossBuilder[-Elem, +To] extends MBuilder[Elem, To] {
  def addOne(elem: Elem): this.type
  def +=(elem: Elem): this.type = addOne(elem)
}

trait CrossFactory[-A, +C] extends Factory[A, C] {
  def fromSpecific(it: IterableOnce[A]): C
  def newBuilder: MBuilder[A, C]

  def apply(from: Nothing): MBuilder[A, C] = newBuilder
  def apply(): MBuilder[A, C] = newBuilder
}

class CrossArraySeqFactory[A: ClassTag] extends Factory[A, ArraySeq[A]] {
  def apply(from: Nothing): MBuilder[A, ArraySeq[A]] = apply()
  def apply(): MBuilder[A, ArraySeq[A]] = CrossArraySeqFactory.newBuilder[A]
}
object CrossArraySeqFactory {
  def newBuilder[A: ClassTag]: MBuilder[A, ArraySeq[A]] =
    new WrappedArrayBuilder[A]().mapResult(wa => ArraySeq.unsafeWrapArray(wa.array))
}