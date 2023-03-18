package com.avsystem.commons
package redis.util


final class SingletonSeq[+A](value: A) extends IIndexedSeq[A] {
  def length: Int = 1
  def apply(idx: Int): A = idx match {
    case 0 => value
    case _ => throw new IndexOutOfBoundsException
  }
}
