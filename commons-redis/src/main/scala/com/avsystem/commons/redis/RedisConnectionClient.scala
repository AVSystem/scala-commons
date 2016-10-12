package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.actor.RedisOperationActor.OpResult
import com.avsystem.commons.redis.actor.{ManagedRedisConnectionActor, RedisOperationActor}
import com.avsystem.commons.redis.config.{ConnectionConfig, ManagedConnectionConfig, NoRetryStrategy}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Author: ghik
  * Created: 09/06/16.
  */
final class RedisConnectionClient(address: NodeAddress = NodeAddress.Default, config: ConnectionConfig = ConnectionConfig())
  (implicit system: ActorSystem) extends RedisConnectionExecutor with Closeable { self =>

  private val connectionActor = system.actorOf(Props(
    new ManagedRedisConnectionActor(address, ManagedConnectionConfig(config, NoRetryStrategy))))

  def executionContext: ExecutionContext =
    system.dispatcher

  def executeBatch[A](batch: RedisBatch[A])(implicit timeout: Timeout): Future[A] =
    connectionActor.ask(batch.rawCommandPacks.requireLevel(Level.Connection, "ConnectionClient"))
      .mapNow({ case pr: PacksResult => batch.decodeReplies(pr) })

  def executeOp[A](op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    system.actorOf(Props(new RedisOperationActor(connectionActor))).ask(op)
      .mapNow({ case or: OpResult[A@unchecked] => or.get })

  def close(): Unit =
    system.stop(connectionActor)
}
