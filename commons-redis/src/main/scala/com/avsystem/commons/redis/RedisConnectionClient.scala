package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Deploy, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.actor.RedisOperationActor.OpResult
import com.avsystem.commons.redis.actor.{ManagedRedisConnectionActor, RedisOperationActor}
import com.avsystem.commons.redis.config.{ConnectionConfig, NoRetryStrategy}
import com.avsystem.commons.redis.exception.ClientStoppedException

import scala.concurrent.{ExecutionContext, Future}

/**
  * Redis client that uses a single, non-reconnectable connection.
  * This is the most "raw" client implementation and the only one capable of directly executing connection state
  * changing commands like `AUTH`, `CLIENT SETNAME`, `WATCH`, etc.
  *
  * However, note that connection-setup commands like `AUTH` may also be specified in
  * [[config.ConnectionConfig ConnectionConfig]]
  * (which may also be specified for connections used by [[RedisNodeClient]] and [[RedisClusterClient]]).
  *
  * This type of client should only be used when requiring capability of manual handling of connection state.
  * If you simply need a single-connection, reconnectable client, use [[RedisNodeClient]] with connection pool size
  * configured to 1.
  */
final class RedisConnectionClient(
  val address: NodeAddress = NodeAddress.Default,
  val config: ConnectionConfig = ConnectionConfig(),
  val actorDeploy: Deploy = Deploy())
  (implicit system: ActorSystem) extends RedisConnectionExecutor with Closeable { self =>

  private val connectionActor = system.actorOf(Props(
    new ManagedRedisConnectionActor(address, config, NoRetryStrategy)).withDeploy(actorDeploy))
  @volatile private[this] var failure = Opt.empty[Throwable]

  private def ifReady[T](code: => Future[T]): Future[T] =
    failure.fold(code)(Future.failed)

  def executionContext: ExecutionContext =
    system.dispatcher

  def executeBatch[A](batch: RedisBatch[A])(implicit timeout: Timeout): Future[A] =
    ifReady(connectionActor.ask(batch.rawCommandPacks.requireLevel(Level.Connection, "ConnectionClient"))
      .mapNow({ case pr: PacksResult => batch.decodeReplies(pr) }))

  def executeOp[A](op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    ifReady(system.actorOf(Props(new RedisOperationActor(connectionActor))).ask(op)
      .mapNow({ case or: OpResult[A@unchecked] => or.get }))

  def close(): Unit = {
    failure = new ClientStoppedException(address.opt).opt
    system.stop(connectionActor)
  }
}
