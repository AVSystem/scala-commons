package com.avsystem.commons
package concurrent

import java.util.concurrent.atomic.AtomicBoolean

object `package` {
  /**
    * A callback that gets notified when value of type `T` gets computed or when computation of that value fails.
    * Callbacks should never throw exceptions. Preferably, they should be simple notifiers that delegate the real
    * work somewhere else, e.g. schedule some handling code on a separate executor
    * (e.g. [[scala.concurrent.ExecutionContext ExecutionContext]]).
    */
  type Callback[T] = Try[T] => Unit

  /**
    * The most low-level, raw type representing an asynchronous, possibly side-effecting operation that yields a
    * value of type `T` as a result.
    * `Async` is a consumer of a callback. When a callback is passed to `Async`, it should start the operation
    * and ultimately notify the callback about the result. Each time the callback is passed, the
    * entire operation should be repeated, involving all possible side effects. Operation should never be started
    * without the callback being passed (i.e. there should be no observable side effects before a callback is passed).
    * Implementation of `Async` should also be prepared to accept a callback before the previous one was notified
    * about the result (i.e. it should support concurrent execution).
    */
  type Async[T] = Callback[T] => Unit
}

object Async {
  private def guarded[T](async: Async[T]): Async[T] = callback => {
    val called = new AtomicBoolean
    val guardedCallback: Callback[T] = result =>
      if (!called.getAndSet(true)) {
        callback(result) // may possibly throw but better let it fly rather than catch and ignore
      }
    try async(guardedCallback) catch {
      case NonFatal(t) =>
        // if callback was already called then we can't do much with the failure, rethrow it
        if (!called.getAndSet(true)) callback(Failure(t)) else throw t
    }
  }

  def safe[T](async: => Async[T]): Async[T] =
    try guarded(async) catch {
      case NonFatal(t) => failed(t)
    }

  def ready[T](result: Try[T]): Async[T] =
    callback => callback(result)

  def successful[T](value: T): Async[T] =
    ready(Success(value))

  def failed[T](cause: Throwable): Async[T] =
    ready(Failure(cause))

  def transform[A, B](async: Async[A])(f: Try[A] => Try[B]): Async[B] =
    cb => async(Callback.contraTransform(cb)(f))

  def map[A, B](async: Async[A])(f: A => B): Async[B] =
    transform(async)(_.map(f))
}

object Callback {
  def contraTransform[A, B](callback: Callback[B])(f: Try[A] => Try[B]): Callback[A] =
    ta => callback(try f(ta) catch {
      case NonFatal(cause) => Failure(cause)
    })

  def contramap[A, B](callback: Callback[B])(f: A => B): Callback[A] =
    contraTransform(callback)(_.map(f))
}
