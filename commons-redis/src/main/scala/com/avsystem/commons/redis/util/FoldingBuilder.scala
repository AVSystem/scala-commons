package com.avsystem.commons
package redis.util

import scala.collection.mutable

class FoldingBuilder[A, B](zero: B, fun: (B, A) => B) extends mutable.Builder[A, B] {
  private[this] var res = zero
  def addOne(elem: A) = {
    res = fun(res, elem)
    this
  }
  def clear(): Unit = res = zero
  def result(): B = res
}

object UnitBuilder extends mutable.Builder[Any, Unit] {
  def addOne(elem: Any) = this
  def clear() = ()
  def result() = ()
}
