package com.avsystem.commons
package concurrent

import com.avsystem.commons.concurrent.AutoPipeliner._
import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.execution.{BufferCapacity, ChannelType, Scheduler}
import monix.reactive.Observable

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.SeqView

object AutoPipeliner {
  final val DefaultMaxBatchSize = 512
  final val DefaultParallelism = 8

  private case class Queued[C, R](cmd: C, result: Promise[R])
}
final class AutoPipeliner[C, R](
  executeBatch: SeqView[C] => Task[IIterable[R]],
  maxBatchSize: Int = DefaultMaxBatchSize,
  parallelism: Int = DefaultParallelism,
)(implicit
  scheduler: Scheduler,
) {
  def exec(cmd: C): Task[R] = for {
    queued <- Task(Queued(cmd, Promise[R]()))
    _ <- queues(roundRobin.getAndIncrement() % parallelism).offer(queued)
    res <- Task.fromFuture(queued.result.future)
  } yield res

  private val roundRobin = new AtomicInteger(0)
  private val queues: IArraySeq[ConcurrentQueue[Task, Queued[C, R]]] =
    IArraySeq.fill(parallelism) {
      ConcurrentQueue[Task]
        .withConfig[Queued[C, R]](BufferCapacity.Bounded(maxBatchSize), ChannelType.MPMC)
        .runSyncUnsafe()
    }

  private def handleBatch(batch: Seq[Queued[C, R]]): Task[Unit] =
    executeBatch(batch.view.map(_.cmd)).materialize.foreachL {
      case Failure(cause) => batch.foreach(_.result.failure(cause))
      case Success(results) =>
        (results.iterator zip batch.iterator.map(_.result)).foreach {
          case (res, promise) => promise.success(res)
        }
    }

  private val runCancelable =
    Task.parTraverseUnordered(queues) { queue =>
      Observable.repeatEvalF(queue.drain(1, maxBatchSize)).mapEval(handleBatch).completedL
    }.runAsync(_ => ())

  def stop(): Unit =
    runCancelable.cancel()
}
