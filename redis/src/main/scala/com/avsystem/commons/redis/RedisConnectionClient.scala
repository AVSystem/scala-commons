package com.avsystem.commons
package redis

import com.avsystem.commons.concurrent.RetryStrategy
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.actor.RedisOperationActor.OpResult
import com.avsystem.commons.redis.actor.{RedisConnectionActor, RedisOperationActor}
import com.avsystem.commons.redis.config.{ConfigDefaults, ConnectionConfig, ExecutionConfig}
import com.avsystem.commons.redis.exception.ClientStoppedException
import org.apache.pekko.actor.{ActorSystem, Props}
import org.apache.pekko.pattern.ask

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
@deprecated("Redis driver is scheduled for removal. It has not been actively tested since v2.21.0. Use a different library, e.g. redisson.", "2.21.0")
final class RedisConnectionClient(
  val address: NodeAddress = NodeAddress.Default,
  val config: ConnectionConfig = ConnectionConfig(),
)
  (implicit system: ActorSystem) extends RedisClient with RedisConnectionExecutor { self =>

  private val initPromise = Promise[Unit]()
  private val connectionActor = {
    val props = Props(new RedisConnectionActor(address, config.copy(reconnectionStrategy = RetryStrategy.never)))
      .withDispatcher(ConfigDefaults.Dispatcher)
    config.actorName.fold(system.actorOf(props))(system.actorOf(props, _))
  }
  connectionActor ! RedisConnectionActor.Open(mustInitiallyConnect = true, initPromise)

  @volatile private[this] var failure = Opt.empty[Throwable]

  private def ifReady[T](code: => Future[T]): Future[T] =
    failure.fold(code)(Future.failed)

  /**
    * Waits until Redis connection is initialized. Note that you can call [[executeBatch]] and [[executeOp]]
    * even if the connection is not yet initialized - requests will be internally queued and executed after
    * initialization is complete.
    */
  def initialized: Future[this.type] =
    initPromise.future.mapNow(_ => this)

  def executionContext: ExecutionContext =
    system.dispatcher

  def executeBatch[A](batch: RedisBatch[A], config: ExecutionConfig): Future[A] =
    ifReady(connectionActor.ask(batch.rawCommandPacks.requireLevel(Level.Connection, "ConnectionClient"))(config.responseTimeout)
      .map({ case pr: PacksResult => batch.decodeReplies(pr) })(config.decodeOn))

  //TODO: don't ignore executionConfig.decodeOn
  def executeOp[A](op: RedisOp[A], executionConfig: ExecutionConfig): Future[A] =
    ifReady(system.actorOf(Props(new RedisOperationActor(connectionActor))).ask(op)(executionConfig.responseTimeout)
      .mapNow({ case or: OpResult[A@unchecked] => or.get }))

  def close(): Unit = {
    val cause = new ClientStoppedException(address.opt)
    failure = cause.opt
    connectionActor ! RedisConnectionActor.Close(cause, stop = true)
  }
}
