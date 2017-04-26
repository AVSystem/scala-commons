package com.avsystem.commons
package redis.examples

import akka.actor.ActorSystem
import akka.util.Timeout
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.exception.{NodeRemovedException, OptimisticLockException}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Example that shows execution of Redis transactions with optimistic locking
  * (involving `WATCH` command and `MULTI`-`EXEC` block). If you simply need to wrap some commands in a
  * `MULTI`-`EXEC` block (without optimistic locking), see [[PipeliningExample]] and [[MultiExecExample]].
  */
object TransactionExample extends App {
  implicit val actorSystem = ActorSystem()

  // In order to execute Redis transaction with optimistic locking, one must execute at least two batches of
  // commands and ensure that they're all executed on the same Redis connection which is exclusively reserved
  // for that transaction. The client must be aware of these requirements and therefore we need to use
  // special type for such transactions - `RedisOp`.

  // `RedisOp` is a sequence of `RedisBatch`es executed one after another on a single, exclusively reserved connection.
  // Each batch in a sequence may depend on the result of previous batch and this is why they can't be merged into a
  // single batch.

  // `RedisOp`s can be created by flat-mapping `RedisBatch`es with each other, e.g. with for-comprehension syntax:
  val api = RedisApi.Batches.StringTyped.valueType[Int]
  // a transaction with optimistic locking which multiplies numeric value under key "key" by 3
  val transaction: RedisOp[Unit] = for {
  // we send WATCH and GET commands in the same batch
    value <- api.watch("key") *> api.get("key").map(_.getOrElse(1))
    // SET command is wrapped in a MULTI-EXEC block
    _ <- api.set("key", value * 3).transaction
  } yield ()


  // RedisOp can be executed by RedisConnectionClient and RedisNodeClient
  val executor: RedisOpExecutor = new RedisNodeClient

  executor.executeOp(transaction).onComplete {
    case Success(()) => println("Transaction successful")
    case Failure(_: OptimisticLockException) =>
      println("Optimistic lock failure!")
    // Someone else concurrently modified the key during our transaction.
    // You must manually retry it, possibly with some backoff mechanism.
    case Failure(t) => t.printStackTrace()
  }

  // Transactions with optimistic locking on RedisClusterClient are a little more tricky.
  // You must manually access RedisNodeClient for appropriate master and also properly handle failures due to
  // cluster state changes.
  val clusterClient = new RedisClusterClient

  def executeTransactionOnCluster(): Future[Unit] =
    clusterClient.initialized.flatMap(client =>
      client.currentState.clientForSlot(api.keySlot("key")).executeOp(transaction))

  executeTransactionOnCluster().onComplete {
    case Success(()) => println("Transaction successful")
    case Failure(_: OptimisticLockException) =>
      println("Optimistic lock failure!")
    // Someone else concurrently modified the key during our transaction.
    // You must manually retry it, possibly with some backoff mechanism
    case Failure(RedirectionException(_) | _: NodeRemovedException) =>
    // Cluster state changed during the transaction, there was a redirection in the middle of it
    // Possibly, the transaction was executed partially, so you must manually fail or retry the transaction
    case Failure(t) => t.printStackTrace()
  }
}
