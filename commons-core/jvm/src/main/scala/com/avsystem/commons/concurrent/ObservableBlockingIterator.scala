package com.avsystem.commons
package concurrent

import java.util.concurrent.ArrayBlockingQueue
import com.avsystem.commons.collection.CloseableIterator
import com.github.ghik.silencer.silent
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber

import scala.concurrent.blocking
import scala.concurrent.duration.TimeUnit

/**
  * An `Iterator` backed by a `BlockingQueue` backed by an `Observable`.
  * This essentially turns an `Observable` into an `Iterator`
  * (which requires blocking so use this only as a last resort).
  */
class ObservableBlockingIterator[T](
  observable: Observable[T],
  timeout: Long,
  unit: TimeUnit,
  bufferSize: Int
)(implicit
  val scheduler: Scheduler
) extends CloseableIterator[T] with Subscriber[T] {

  import ObservableBlockingIterator._

  private[this] var last: Any = Empty

  @volatile private[this] var ackPromise: Promise[Ack] = _
  private val queue = new ArrayBlockingQueue[Any](bufferSize)
  private val cancelable = observable.subscribe(this)

  def onNext(elem: T): Future[Ack] = {
    // checking size is safe because only `onNext/onError/onComplete` add to the queue
    // and they are guaranteed to be invoked sequentially
    if (queue.remainingCapacity > 1) {
      // there's more than one spot in the queue, add this element and acknowledge immediately
      queue.add(elem)
      Ack.Continue
    } else {
      // not sure if there's more than one spot in the queue - add the element but return a Promise-backed Future of acknowledgement
      // NOTE: the Observable protocol guarantees that `onNext/onError/onComplete` is never called when the queue is full
      val promise = Promise[Ack]()
      ackPromise = promise
      queue.add(elem)
      // must use promise from local val because `fetchNext()` may have already erased `ackPromise`
      promise.future
    }
  }

  def onError(ex: Throwable): Unit =
    queue.add(Failed(ex))

  def onComplete(): Unit =
    queue.add(Complete)

  private def fetchNext(): Any = last match {
    case Empty =>
      last = blocking(queue.poll(timeout, unit))
      val promise = ackPromise
      // after the queue got full, wait until at least half of its capacity is free before letting
      // the Observable produce more elements
      if (promise != null && queue.remainingCapacity >= bufferSize / 2) {
        promise.success(Ack.Continue)
        ackPromise = null
      }
      last
    case nonEmpty =>
      nonEmpty
  }

  @silent("non-nullary method overrides nullary method")
  def hasNext: Boolean = fetchNext() match {
    case Complete => false
    case Failed(cause) => throw cause
    case _ => true
  }

  def next(): T = fetchNext() match {
    case Complete => throw new NoSuchElementException
    case Failed(cause) => throw cause
    case value: T@unchecked =>
      last = Empty
      value
  }

  def close(): Unit =
    cancelable.cancel()
}
object ObservableBlockingIterator {
  private object Empty
  private object Complete
  private case class Failed(ex: Throwable)
}
