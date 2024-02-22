package com.avsystem.commons
package concurrent

import com.avsystem.commons.concurrent.TaskExtensions.{TaskCompanionOps, TaskOps}
import com.avsystem.commons.misc.Timestamp
import monix.eval.Task

import java.util.concurrent.TimeUnit
import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

trait TaskExtensions {
  implicit def taskOps[T](task: Task[T]): TaskOps[T] = new TaskOps(task)

  implicit def taskCompanionOps(task: Task.type): TaskCompanionOps.type = TaskCompanionOps
}

object TaskExtensions extends TaskExtensions {
  final class TaskOps[T](private val task: Task[T]) extends AnyVal {
    /**
      * Like regular `timeout` but [[TimeoutException]] is created lazily (for performance).
      */
    def lazyTimeout(after: FiniteDuration, msg: => String): Task[T] =
      task.timeoutTo(after, Task.raiseError(new TimeoutException(msg)))

    /**
      * Similar to [[Task.tapEval]], accepts simple consumer function as an argument
      */
    def tapL(f: T => Unit): Task[T] =
      task.map(_.setup(f))

    /**
      * Similar to [[Task.tapError]], accepts [[PartialFunction]] as an argument
      */
    def tapErrorL[B](f: PartialFunction[Throwable, B]): Task[T] =
      task.tapError(t => Task(f.applyOpt(t)))
  }

  object TaskCompanionOps {
    /** A [[Task]] of [[Opt.Empty]] */
    def optEmpty[A]: Task[Opt[A]] = Task.pure(Opt.Empty)

    def traverseOpt[A, B](opt: Opt[A])(f: A => Task[B]): Task[Opt[B]] =
      opt.fold(Task.optEmpty[B])(a => f(a).map(_.opt))

    def fromOpt[A](maybeTask: Opt[Task[A]]): Task[Opt[A]] = maybeTask match {
      case Opt(task) => task.map(_.opt)
      case Opt.Empty => optEmpty
    }

    def currentTimestamp: Task[Timestamp] =
      Task.clock.realTime(TimeUnit.MILLISECONDS).map(Timestamp(_))

    def usingNow[T](useNow: Timestamp => Task[T]): Task[T] =
      currentTimestamp.flatMap(useNow)
  }
}
