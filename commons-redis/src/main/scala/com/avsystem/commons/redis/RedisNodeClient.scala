package com.avsystem.commons
package redis

import java.io.Closeable
import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ActorRef, ActorSystem, Deploy, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor.{Close, PacksResult}
import com.avsystem.commons.redis.actor.RedisOperationActor.OpResult
import com.avsystem.commons.redis.actor.{RedisConnectionActor, RedisOperationActor}
import com.avsystem.commons.redis.config.{ConnectionConfig, NodeConfig}
import com.avsystem.commons.redis.exception.{ClientStoppedException, NodeRemovedException}

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * Redis client implementation for a single Redis node using a connection pool. Connection pool size is constant
  * and batches and operations are distributed over connections using round-robin scheme. Connections are automatically
  * reconnected upon failure (possibly with an appropriate delay, see [[config.NodeConfig NodeConfig]]
  * for details).
  */
final class RedisNodeClient(
  val address: NodeAddress = NodeAddress.Default,
  val config: NodeConfig = NodeConfig(),
  val actorDeploy: Deploy = Deploy())
  (implicit system: ActorSystem) extends RedisNodeExecutor with Closeable { client =>

  private def createConnection(i: Int) = {
    val connConfig: ConnectionConfig = config.connectionConfigs(i)
    val props = Props(new RedisConnectionActor(address, connConfig, config.reconnectionStrategy)).withDeploy(actorDeploy)
    connConfig.actorName.fold(system.actorOf(props))(system.actorOf(props, _))
  }

  private val connections = (0 until config.poolSize).iterator.map(createConnection).toArray
  private val index = new AtomicLong(0)

  @volatile private[this] var initSuccess = false
  @volatile private[this] var failure = Opt.empty[Throwable]
  private val initFuture = Promise[Any]()
    .completeWith(executeOp(connections(0), config.initOp)(config.initTimeout)).future
  initFuture.onSuccess({ case _ => initSuccess = true })(RunNowEC)

  private def ifReady[T](code: => Future[T]): Future[T] =
    failure.fold(if (initSuccess) code else initFuture.flatMapNow(_ => code))(Future.failed)

  private def nextConnection() =
    connections((index.getAndIncrement() % config.poolSize).toInt)

  private[redis] def executeRaw(packs: RawCommandPacks)(implicit timeout: Timeout): Future[PacksResult] =
    ifReady(nextConnection().ask(packs).mapTo[PacksResult])

  /**
    * Notifies the [[RedisNodeClient]] that its node is no longer a master in Redis Cluster and.
    * The client stops itself as a result and fails any pending unsent requests with
    * [[exception.NodeRemovedException]].
    */
  private[redis] def nodeRemoved(): Unit = {
    val cause = new NodeRemovedException(address)
    failure = cause.opt
    connections.foreach(_ ! Close(cause))
  }

  def executionContext: ExecutionContext =
    system.dispatcher

  /**
    * Executes a [[RedisBatch]] on this client by sending its commands to the Redis node in a single network
    * message (technically, a single `akka.io.Tcp.Write` message). Therefore it's also naturally guaranteed that
    * all commands in a batch are executed on the same connection.
    *
    * Note that even though connection used by [[RedisNodeClient]] are automatically reconnected, it's still possible
    * that an error is returned for some batches that were executed around the time connection failure happened.
    * To be precise, [[exception.ConnectionClosedException ConnectionClosedException]]
    * is returned for batches that were sent through the connection but the connection failed before
    * a response could be received. There is no way to safely retry such batches because it is unknown whether Redis
    * node actually received and executed them or not.
    *
    * Execution of each command in the batch or the whole batch may fail due to following reasons:
    * <ul>
    * <li>[[exception.ForbiddenCommandException ForbiddenCommandException]] when trying to execute command not
    * supported by this client type</li>
    * <li>[[exception.ErrorReplyException ErrorReplyException]] when Redis server replies with an error for some command.
    * In particular, if this client was obtained from [[RedisClusterClient]] then every keyed command may fail with
    * cluster redirection. You can pattern-match redirection errors using [[RedirectionException]] extractor object.
    * </li>
    * <li>[[exception.UnexpectedReplyException UnexpectedReplyException]] when Redis server replies with something
    * unexpected by a decoder of some command</li>
    * <li>[[exception.ConnectionClosedException ConnectionClosedException]] when connection is closed or
    * reset (the client reconnects automatically after connection failure but commands that were in the middle of
    * execution may still fail)</li>
    * <li>[[exception.WriteFailedException WriteFailedException]] when a network write
    * failed</li>
    * <li>[[exception.NodeRemovedException NodeRemovedException]] when this client was obtained from
    * [[RedisClusterClient]] and Redis node that it's connected to is no longer a master</li>
    * </ul>
    */
  def executeBatch[A](batch: RedisBatch[A])(implicit timeout: Timeout): Future[A] =
    executeRaw(batch.rawCommandPacks).mapNow(result => batch.decodeReplies(result))

  /**
    * Executes a [[RedisOp]] on this client. [[RedisOp]] is a sequence of dependent [[RedisBatch]]es,
    * that requires an exclusive access to a single Redis connection. Typically, a [[RedisOp]] is a
    * `WATCH`-`MULTI`-`EXEC` transaction (see [[RedisOp]] for more details).
    *
    * Note that the client does not handle optimistic lock failures (which happen when watched key is modified by
    * other client). An [[exception.OptimisticLockException OptimisticLockException]] is
    * returned in such cases and you must recover from it manually.
    *
    * Execution of a [[RedisOp]] may also fail for the same reasons as specified for [[RedisBatch]] in [[executeBatch]].
    * Be especially careful when using node clients obtained from cluster client.
    */
  def executeOp[A](op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    ifReady(executeOp(nextConnection(), op))

  private def executeOp[A](connection: ActorRef, op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    system.actorOf(Props(new RedisOperationActor(connection))).ask(op)
      .mapNow({ case or: OpResult[A@unchecked] => or.get })

  def initialized: Future[this.type] =
    initFuture.mapNow(_ => this)

  def close(): Unit = {
    failure = failure orElse new ClientStoppedException(address.opt).opt
    connections.foreach(system.stop)
  }
}
