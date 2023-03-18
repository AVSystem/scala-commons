package com.avsystem.commons
package misc

import scala.runtime.ScalaRunTime

/**
  * Implements common case class & case object methods normally synthesized by the compiler. Extending this trait by
  * case class or case object prevents the compiler from synthesizing these methods which can reduce generated JS size
  * at penalty of not-exactly-precise implementation of `canEqual` and `equals` and its runtime performance.
  * For this reason, non-abstract classes extending this trait should always be final.
  * If possible, prefer using [[AbstractCase]] rather than this trait.
  */
trait CaseMethods extends Any with Product {
  override def hashCode: Int = ScalaRunTime._hashCode(this)
  override def equals(obj: Any): Boolean = obj match {
    case self if self.asInstanceOf[AnyRef] eq this.asInstanceOf[AnyRef] => true
    case prod: Product => canEqual(prod) && productIterator.sameElements(prod.productIterator)
    case _ => false
  }
  override def toString: String = ScalaRunTime._toString(this)
  override def productIterator: Iterator[Any] = super.productIterator
  def canEqual(that: Any): Boolean = getClass.isAssignableFrom(that.getClass)
}

/**
  * Base class for case classes that reduces amount of code that the compiler generates for them.
  * Useful primarily for JS size reduction. See [[CaseMethods]] for more details.
  */
abstract class AbstractCase extends CaseMethods
