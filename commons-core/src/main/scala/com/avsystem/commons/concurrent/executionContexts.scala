package com.avsystem.commons
package concurrent

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

object RunNowEC extends ExecutionContext {
  object Implicits {
    implicit val executionContext: ExecutionContext = RunNowEC
  }

  def execute(runnable: Runnable): Unit =
    runnable.run()

  def reportFailure(cause: Throwable): Unit =
    cause.printStackTrace()
}

object RunInQueueEC extends ExecutionContext {
  object Implicits {
    implicit val executionContext: ExecutionContext = RunInQueueEC
  }

  private val queueTL = new ThreadLocal[mutable.Queue[Runnable]] {
    override def initialValue = new mutable.Queue[Runnable]
  }

  def execute(runnable: Runnable): Unit = {
    val queue = queueTL.get
    val shouldRun = queue.isEmpty
    queue += runnable
    if (shouldRun) {
      while (queue.nonEmpty) {
        val task = queue.head
        try task.run() catch {
          case NonFatal(t) => reportFailure(t)
        }
        queue.dequeue()
      }
    }
  }

  def reportFailure(cause: Throwable): Unit =
    cause.printStackTrace()
}

trait HasExecutionContext {
  protected implicit def executionContext: ExecutionContext
}

trait HasRunNowEC extends HasExecutionContext {
  protected implicit final def executionContext: ExecutionContext = RunNowEC
}

trait HasRunInQueueEC extends HasExecutionContext {
  protected implicit final def executionContext: ExecutionContext = RunInQueueEC
}
