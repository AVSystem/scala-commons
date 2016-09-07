package com.avsystem.commons
package redis

import java.io.Closeable
import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.redis.actor.ManagedRedisConnectionActor.NodeRemoved
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
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

  private val connections = (0 until config.poolSize).iterator.map(createConnection).toArray
  private val index = new AtomicLong(0)
  private val initFuture = Promise[Any]()
    .completeWith(executeOp(connections(0), config.initOp)(config.initTimeout)).future

  private def nextConnection() =
    connections((index.getAndIncrement() % config.poolSize).toInt)

  private[redis] def executeRaw(packs: RawCommandPacks)(implicit timeout: Timeout): Future[PacksResult] =
    initFuture.flatMapNow(_ => nextConnection().ask(packs)).mapTo[PacksResult]

  /**
    * Notifies the [[RedisNodeClient]] that its node is no longer a master in Redis Cluster and.
    * The client stops itself as a result and fails any pending requests with
    * [[com.avsystem.commons.redis.exception.NodeRemovedException]].
    */
  private[redis] def nodeRemoved(): Unit = connections.foreach(_ ! NodeRemoved)

  def executeBatch[A](batch: RedisBatch[A])(implicit timeout: Timeout): Future[A] =
    executeRaw(batch.rawCommandPacks).mapNow(result => batch.decodeReplies(result))

  def executeOp[A](op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    initFuture.flatMapNow(_ => executeOp(nextConnection(), op))

  private def executeOp[A](connection: ActorRef, op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    system.actorOf(Props(new RedisOperationActor(connection))).ask(op)
      .mapNow({ case or: OpResult[A@unchecked] => or.get })

  def initialized: Future[this.type] =
    initFuture.mapNow(_ => this)

  def toExecutor(implicit timeout: Timeout): RedisNodeExecutor =
    new RedisNodeExecutor {
      def execute[A](cmd: RedisBatch[A]) = client.executeBatch(cmd)
    }

  def close(): Unit =
    connections.foreach(system.stop)
}
