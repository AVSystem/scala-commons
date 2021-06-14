package com.avsystem.commons
package collection

import java.util.function.Function
import scala.annotation.nowarn
import scala.annotation.unchecked.uncheckedVariance

/**
  * Abstraction over simple `Iterator` that allows one to close the resources associated with iterator without
  * iterating through the whole result set.
  *
  * Note: This is both Java and Scala `Iterator`.
  */
trait CloseableIterator[+T] extends Iterator[T] with JIterator[T@uncheckedVariance] with AutoCloseable {
  def transform[V](transform: Iterator[T] => Iterator[V]): CloseableIterator[V] =
    CloseableIterator.apply(transform(this), this)

  def jTransform[V](transform: Function[JIterator[T@uncheckedVariance], JIterator[V]]): CloseableIterator[V] =
    CloseableIterator.apply(transform(this), this)
}
object CloseableIterator {
  private object emptyCloseable extends AutoCloseable {
    def close(): Unit = ()
  }

  def empty[T]: CloseableIterator[T] =
    noop(Iterator.empty)

  def noop[T](it: Iterator[T]): CloseableIterator[T] =
    apply(it, emptyCloseable)

  def noop[T](it: JIterator[T]): CloseableIterator[T] =
    apply(it, emptyCloseable)

  def apply[T](it: JIterator[T] with AutoCloseable): CloseableIterator[T] =
    apply(it, it)

  def apply[T](it: JIterator[T], closeable: AutoCloseable): CloseableIterator[T] =
    new CloseableIterator[T] {
      def close(): Unit = closeable.close()
      @nowarn("msg=non-nullary method overrides nullary method")
      def hasNext: Boolean = it.hasNext
      def next(): T = it.next()
    }

  def apply[T](it: Iterator[T] with AutoCloseable): CloseableIterator[T] =
    apply(it, it)

  def apply[T](it: Iterator[T], closeable: AutoCloseable): CloseableIterator[T] =
    new CloseableIterator[T] {
      def close(): Unit = closeable.close()
      @nowarn("msg=non-nullary method overrides nullary method")
      def hasNext: Boolean = it.hasNext
      def next(): T = it.next()
    }
}
