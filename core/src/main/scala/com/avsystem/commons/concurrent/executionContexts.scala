package com.avsystem.commons
package concurrent

import scala.collection.mutable
import scala.concurrent.ExecutionContextExecutor

/**
 * DO NOT USE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.
 *
 * This execution context runs callbacks immediately in whatever happens to be current thread. This may help with
 * performance but is otherwise very unsafe. It may introduce unwanted load or block some crucial threads in the
 * system. This may even collapse the entire system in pessimistic case.
 *
 * Note: Since Scala 2.13, this is essentially the same thing as [[ExecutionContext.parasitic]].
 */
object RunNowEC extends ExecutionContextExecutor {
  def get: ExecutionContextExecutor = this

  object Implicits {
    implicit val executionContext: ExecutionContext = RunNowEC
  }

  def execute(runnable: Runnable): Unit =
    runnable.run()

  def reportFailure(cause: Throwable): Unit =
    cause.printStackTrace()
}

object RunInQueueEC extends RunInQueueEC {
  def get: ExecutionContextExecutor = this

  object Implicits {
    implicit val executionContext: ExecutionContext = RunInQueueEC
  }
}

/**
 * DO NOT USE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.
 *
 * This execution context runs callbacks immediately in whatever happens to be current thread, additionally maintaining
 * a queue that may prevent stack from growing.
 *
 * This may help with performance but is otherwise very unsafe.
 */
class RunInQueueEC extends ExecutionContextExecutor {
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
        try task.run()
        catch {
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
