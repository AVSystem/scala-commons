package com.avsystem.commons
package concurrent

import com.avsystem.commons.concurrent.TaskExtensions.{TaskCompanionOps, TaskOps}
import com.avsystem.commons.misc.Timestamp
import monix.eval.Task
import monix.reactive.Observable

import java.util.concurrent.TimeUnit
import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

trait TaskExtensions {
  implicit def taskOps[T](task: Task[T]): TaskOps[T] = new TaskOps(task)

  implicit def taskCompanionOps(task: Task.type): TaskCompanionOps.type = TaskCompanionOps
}

object TaskExtensions extends TaskExtensions {
  final class TaskOps[T](private val task: Task[T]) extends AnyVal {

    /** Similar to [[Task.timeoutWith]] but exception instance is created lazily (for performance)
      */
    inline def lazyTimeout(after: FiniteDuration, inline msg: => String): Task[T] =
      task.timeoutTo(after, Task.defer(Task.raiseError(new TimeoutException(msg))))

    /** Similar to [[Task.tapEval]], accepts simple consumer function as an argument
      */
    inline def tapL(inline f: T => Unit): Task[T] =
      task.map(_.setup(f))

    /** Similar to [[Task.tapError]], accepts [[PartialFunction]] as an argument
      */
    inline def tapErrorL[B](inline f: PartialFunction[Throwable, B]): Task[T] =
      task.tapError(t => Task(f.applyOpt(t)))
  }

  object TaskCompanionOps {
    import com.avsystem.commons.concurrent.ObservableExtensions.observableOps

    /** A [[Task]] of [[Opt.Empty]] */
    def optEmpty[A]: Task[Opt[A]] = Task.pure(Opt.Empty)

    inline def traverseOpt[A, B](opt: Opt[A])(inline f: A => Task[B]): Task[Opt[B]] =
      opt.fold(Task.optEmpty[B])(a => f(a).map(_.opt))

    def fromOpt[A](maybeTask: Opt[Task[A]]): Task[Opt[A]] = maybeTask match {
      case Opt(task) => task.map(_.opt)
      case Opt.Empty => Task.optEmpty
    }

    inline def traverseMap[K, V, A, B](map: Map[K, V])(inline f: (K, V) => Task[(A, B)]): Task[Map[A, B]] =
      Observable.fromIterable(map).mapEval { case (key, value) => f(key, value) }.toL(Map)

    inline def traverseMapValues[K, A, B](map: Map[K, A])(inline f: (K, A) => Task[B]): Task[Map[K, B]] =
      traverseMap(map) { case (key, value) => f(key, value).map(key -> _) }

    def currentTimestamp: Task[Timestamp] =
      Task.clock.realTime(TimeUnit.MILLISECONDS).map(Timestamp(_))

    inline def usingNow[T](inline useNow: Timestamp => Task[T]): Task[T] =
      currentTimestamp.flatMap(useNow)
  }
}
