package com.avsystem.commons
package redis

import java.io.Closeable
import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.redis.Scope.Node
import com.avsystem.commons.redis.actor.RedisConnectionActor.BatchResult
import com.avsystem.commons.redis.actor.RedisOperationActor.OpResult
import com.avsystem.commons.redis.actor.{ManagedRedisConnectionActor, RedisOperationActor}

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 05/04/16.
  */
final class RedisNodeClient(address: NodeAddress = NodeAddress.Default, poolSize: Int = 1)
  (implicit system: ActorSystem) extends Closeable {client =>

  private val connections = Array.fill(poolSize)(system.actorOf(Props(new ManagedRedisConnectionActor(address))))
  private val index = new AtomicLong(0)

  private def nextConnection() =
    connections((index.getAndIncrement() % poolSize).toInt)

  def executeBatch[A](batch: NodeBatch[A])(implicit timeout: Timeout): Future[A] =
    nextConnection().ask(batch).mapNow({ case br: BatchResult[A@unchecked] => br.get })

  def executeOp[A](op: RedisOp[A])(implicit timeout: Timeout): Future[A] = {
    val connection = nextConnection()
    system.actorOf(Props(new RedisOperationActor(connection))).ask(op)
      .mapNow({ case or: OpResult[A@unchecked] => or.get })
  }

  //TODO possible optimized version for batches / LeafOps

  def toExecutor(implicit timeout: Timeout): RedisExecutor[Node] =
    new RedisExecutor[Node] {
      def execute[A](cmd: NodeBatch[A]) = client.executeBatch(cmd)
    }

  def close(): Unit =
    connections.foreach(system.stop)
}
