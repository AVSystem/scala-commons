package com.avsystem.commons
package collection

import scala.collection.Factory
import scala.collection.immutable.ArraySeq

trait CrossBuilder[-Elem, +To] extends MBuilder[Elem, To]
trait CrossFactory[-A, +C] extends Factory[A, C]

class CrossArraySeqFactory[A: ClassTag] extends Factory[A, ArraySeq[A]] {
  def fromSpecific(it: IterableOnce[A]): ArraySeq[A] = {
    val b = newBuilder
    b.addAll(it)
    b.result()
  }

  def newBuilder: MBuilder[A, ArraySeq[A]] = CrossArraySeqFactory.newBuilder
}
object CrossArraySeqFactory {
  def newBuilder[A: ClassTag]: MBuilder[A, ArraySeq[A]] = ArraySeq.newBuilder[A]
}