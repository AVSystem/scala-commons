package com.avsystem.commons
package redis.util

import com.avsystem.commons.collection.CrossBuilder

class FoldingBuilder[A, B](zero: B, fun: (B, A) => B) extends CrossBuilder[A, B] {
  private[this] var res = zero
  def addOne(elem: A): this.type = {
    res = fun(res, elem)
    this
  }
  def clear(): Unit = res = zero
  def result(): B = res
}

object UnitBuilder extends CrossBuilder[Any, Unit] {
  def addOne(elem: Any): this.type = this
  def clear(): Unit = ()
  def result(): Unit = ()
}
