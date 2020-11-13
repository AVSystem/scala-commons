package com.avsystem.commons
package collection

import java.util.function.Function

import com.github.ghik.silencer.silent

import scala.annotation.unchecked.uncheckedVariance

/**
  * Abstraction over simple [[Iterator]] that allows one to close the resources associated with iterator without
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
  def empty[T]: CloseableIterator[T] =
    noop(Iterator.empty)

  def noop[T](iterator: Iterator[T]): CloseableIterator[T] =
    apply(iterator, () => ()) // AutoCloseable implemented with zero-argument lambda that returns Unit

  def noop[T](iterator: JIterator[T]): CloseableIterator[T] =
    apply(iterator, () => ()) // AutoCloseable implemented with zero-argument lambda that returns Unit

  def apply[T](iterator: JIterator[T] with AutoCloseable): CloseableIterator[T] =
    apply(iterator, iterator)

  def apply[T](iterator: JIterator[T], closeable: AutoCloseable): CloseableIterator[T] =
    new CloseableIterator[T] {
      def close(): Unit = closeable.close()
      @silent("non-nullary method overrides nullary method")
      def hasNext: Boolean = iterator.hasNext
      def next(): T = iterator.next()
    }

  def apply[T](iterator: Iterator[T] with AutoCloseable): CloseableIterator[T] =
    apply(iterator, iterator)

  def apply[T](iterator: Iterator[T], closeable: AutoCloseable): CloseableIterator[T] =
    new CloseableIterator[T] {
      def close(): Unit = closeable.close()
      @silent("non-nullary method overrides nullary method")
      def hasNext: Boolean = iterator.hasNext
      def next(): T = iterator.next()
    }
}
