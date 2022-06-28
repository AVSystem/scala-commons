package com.avsystem.commons
package concurrent

import com.avsystem.commons.collection.CloseableIterator
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.bio.IO

import scala.concurrent.Await
import scala.concurrent.duration._

abstract class BlockingUtils {
  def defaultTimeout: Duration = 60.seconds
  def defaultBufferSize: Int = 128

  /**
    * Default scheduler used to run `Task`s and `Observable`s.
    * This scheduler is not meant for blocking code.
    */
  implicit def scheduler: Scheduler

  /**
    * Scheduler used for running blocking code.
    */
  def ioScheduler: Scheduler

  /**
    * Wraps blocking code into a [[Future]], making sure that blocking happens on an unbounded thread pool
    * meant specifically for that purpose.
    */
  def asFuture[T](blockingCode: => T): Future[T] =
    Future(blockingCode)(ioScheduler)

  /**
    * Wraps blocking code into a `Task`, making sure that blocking happens on an unbounded thread pool
    * meant specifically for that purpose.
    */
  def asTask[T](blockingCode: => T): Task[T] =
    Task.eval(blockingCode).executeOn(ioScheduler, forceAsync = true)

  def asIO[A, B](blockingCode: => Either[A, B]): IO[A, B] = IO.deferTotal {
    IO.fromEither(blockingCode)
  }.executeOn(ioScheduler, forceAsync = true)

  def await[T](future: Future[T]): T =
    await(future, defaultTimeout)

  def await[T](future: Future[T], timeout: Long, unit: TimeUnit): T =
    await(future, FiniteDuration(timeout, unit))

  def await[T](future: Future[T], timeout: Duration): T =
    Await.result(future, timeout)

  // overloading instead of using default value so that it's usable from Java
  def runAndAwait[T](task: Task[T]): T =
    runAndAwait(task, defaultTimeout)

  def runAndAwait[T](task: Task[T], timeout: Long, unit: TimeUnit): T =
    runAndAwait(task, FiniteDuration(timeout, unit))

  def runAndAwait[T](task: Task[T], timeout: Duration): T =
    task.executeAsync.runSyncUnsafe(timeout)

  // overloading instead of using default value so that it's usable from Java
  def toIterator[T](observable: Observable[T]): CloseableIterator[T] =
    new ObservableBlockingIterator[T](observable, defaultTimeout.length, defaultTimeout.unit, defaultBufferSize)

  def toIterator[T](observable: Observable[T], nextElementTimeout: Duration): CloseableIterator[T] =
    new ObservableBlockingIterator[T](observable, nextElementTimeout.length, nextElementTimeout.unit, defaultBufferSize)

  def toIterator[T](observable: Observable[T], nextElementTimeout: Long, unit: TimeUnit): CloseableIterator[T] =
    new ObservableBlockingIterator[T](observable, nextElementTimeout, unit, defaultBufferSize)

  def toIterator[T](observable: Observable[T], nextElementTimeout: Duration, bufferSize: Int): CloseableIterator[T] =
    new ObservableBlockingIterator[T](observable, nextElementTimeout.length, nextElementTimeout.unit, bufferSize)

  def toIterator[T](observable: Observable[T], nextElementTimeout: Long, unit: TimeUnit, bufferSize: Int): CloseableIterator[T] =
    new ObservableBlockingIterator[T](observable, nextElementTimeout, unit, bufferSize)
}

object DefaultBlocking extends BlockingUtils {
  implicit def scheduler: Scheduler = Scheduler.global
  def ioScheduler: Scheduler = Scheduler.io()
}
