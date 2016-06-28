package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.redis.Scope.Connection
import com.avsystem.commons.redis.actor.RedisConnectionActor
import com.avsystem.commons.redis.actor.RedisConnectionActor.BatchResult
import com.avsystem.commons.redis.config.ConnectionConfig

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 09/06/16.
  */
final class RedisConnectionClient(address: NodeAddress = NodeAddress.Default, config: ConnectionConfig = ConnectionConfig())
  (implicit system: ActorSystem) extends Closeable {self =>

  private val connectionActor = system.actorOf(Props(new RedisConnectionActor(address, config)))

  def execute[A](batch: ConnectionBatch[A])(implicit timeout: Timeout): Future[A] =
    connectionActor.ask(batch).mapNow({ case br: BatchResult[A@unchecked] => br.get })

  def toExecutor(implicit timeout: Timeout): RedisExecutor[Connection] =
    new RedisExecutor[Connection] {
      def execute[A](batch: RedisBatch[A, Connection]) = self.execute(batch)
    }

  def close(): Unit =
    system.stop(connectionActor)
}
