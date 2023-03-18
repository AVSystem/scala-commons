package com.avsystem.commons
package redis.util

final class HeadIterable[+A](head: A, tail: Iterable[A]) extends Iterable[A] {
  def iterator = new HeadIterator(head, tail.iterator)

  override def isEmpty = false
  override def foreach[U](f: A => U): Unit = {
    f(head)
    tail.foreach(f)
  }
}

final class HeadIterator[+A](head: A, tail: Iterator[A]) extends Iterator[A] {
  private[this] var atHead = true
  def hasNext: Boolean = atHead || tail.hasNext
  def next(): A =
    if (atHead) {
      atHead = false
      head
    } else tail.next()
}
