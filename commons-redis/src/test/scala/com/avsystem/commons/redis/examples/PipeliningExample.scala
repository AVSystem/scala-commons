package com.avsystem.commons
package redis.examples

import akka.actor.ActorSystem
import com.avsystem.commons.redis._

// Global execution context is used for the sake of simplicity of this example,
// think well if this is what you actually want.
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Examples showing how to create and execute batches made of multiple Redis commands.
  */
object PipeliningExample extends App {
  implicit val actorSystem: ActorSystem = ActorSystem()

  // Pipelining is a technique in which multiple Redis commands are sent to server at once, without one command
  // waiting for previous one to finish. This allows sending multiple commands in a single network message, which
  // is beneficial for performance.
  // In case of Redis Cluster deployment, batch may be divided into multiple sub-batches, each one sent to different
  // master node.

  // An API object whose methods return RedisBatch objects, which are representations of not-yet-executed Redis commands
  // which can be combined with each other to form larger batches.
  val api = RedisApi.Batches.StringTyped

  val executor: RedisExecutor = new RedisNodeClient

  // In order to send a few commands in a single batch and retrieve all results, create a tuple of batches
  // and call extension method `sequence` on it, which will transform the tuple of batches into a single batch
  // which returns a tuple
  val pairBatch: RedisBatch[(Opt[String], Long)] = (api.get("key"), api.incr("otherKey")).sequence
  executor.executeBatch(pairBatch).onComplete {
    case Success((Opt(textValue), numericValue)) => println(s"Got $textValue and $numericValue")
    case Success((Opt.Empty, numericValue)) => println(s"Got only $numericValue")
    case Failure(t) => t.printStackTrace()
  }

  // When merging two batches with each other, you can discard result of one of them by using *> or <* operator
  // In this case we're not interested in result of SET command, only INCR
  val singleResultBatch: RedisBatch[Long] =
  api.set("key", "value") *> api.incr("otherKey")

  // `sequence` works not only on tuples but also on collections
  // In this case we're sending 100 INCR commands in a single batch and get the result in a collection
  val collectionBatch: RedisBatch[Seq[Long]] =
  (0 until 100).map(i => api.incr(s"key$i")).sequence

  // You can have both tuples and collections in a single batch, arbitrarily nested:
  val compositeBatch: RedisBatch[(Opt[String], Seq[Long])] =
    (api.get("key"), (0 until 100).map(i => api.incr(s"key$i"))).sequence

  // NOTE: when using `executeBatch`, the type system does not protect you from sending commands not supported by
  // particular client type. For example, the code below will compile, but execution will fail with
  // `ForbiddenCommandException`
  executor.executeBatch(api.clientSetname("name")) // ForbiddedCommandException, can't execute CLIENT SETNAME using RedisNodeClient
}
