package com.avsystem.commons
package redis

import java.io.Closeable
import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.redis.Scope.Node
import com.avsystem.commons.redis.actor.RedisConnectionActor.BatchResult
import com.avsystem.commons.redis.actor.RedisOperationActor.OpResult
import com.avsystem.commons.redis.actor.{ManagedRedisConnectionActor, RedisOperationActor}
import com.avsystem.commons.redis.config.NodeConfig

import scala.concurrent.{Future, Promise}

final class RedisNodeClient(
  val address: NodeAddress = NodeAddress.Default,
  val config: NodeConfig = NodeConfig())
  (implicit system: ActorSystem) extends Closeable {client =>

  private def createConnection(i: Int) =
    system.actorOf(Props(new ManagedRedisConnectionActor(address, config.connectionConfigs(i))))

  private val connections = (0 to config.poolSize).iterator.map(createConnection).toArray
  private val index = new AtomicLong(0)
  private val initFuture = Promise[Any]()
    .completeWith(executeOp(connections(0), config.initOp)(config.initTimeout)).future

  private def nextConnection() =
    connections((index.getAndIncrement() % config.poolSize).toInt)

  def executeBatch[A](batch: NodeBatch[A])(implicit timeout: Timeout): Future[A] =
    initFuture.flatMapNow(_ => nextConnection().ask(batch).mapNow({ case br: BatchResult[A@unchecked] => br.get }))

  def executeOp[A](op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    initFuture.flatMapNow(_ => executeOp(nextConnection(), op))

  private def executeOp[A](connection: ActorRef, op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    system.actorOf(Props(new RedisOperationActor(connection))).ask(op)
      .mapNow({ case or: OpResult[A@unchecked] => or.get })

  def initialized: Future[this.type] =
    initFuture.mapNow(_ => this)

  def toExecutor(implicit timeout: Timeout): RedisExecutor[Node] =
    new RedisExecutor[Node] {
      def execute[A](cmd: NodeBatch[A]) = client.executeBatch(cmd)
    }

  def close(): Unit =
    connections.foreach(system.stop)
}
