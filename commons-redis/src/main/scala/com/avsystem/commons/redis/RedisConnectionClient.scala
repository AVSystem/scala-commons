package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.actor.RedisConnectionActor
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.config.ConnectionConfig

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 09/06/16.
  */
final class RedisConnectionClient(address: NodeAddress = NodeAddress.Default, config: ConnectionConfig = ConnectionConfig())
  (implicit system: ActorSystem) extends Closeable {self =>

  private val connectionActor = system.actorOf(Props(new RedisConnectionActor(address, config)))

  def execute[A](batch: RedisBatch[A])(implicit timeout: Timeout): Future[A] =
    connectionActor.ask(batch.rawCommandPacks.requireLevel(Level.Connection, "ConnectionClient"))
      .mapNow({ case pr: PacksResult => batch.decodeReplies(pr) })

  def toExecutor(implicit timeout: Timeout): RedisConnectionExecutor =
    new RedisConnectionExecutor {
      def execute[A](batch: RedisBatch[A]) = self.execute(batch)
    }

  def close(): Unit =
    system.stop(connectionActor)
}
