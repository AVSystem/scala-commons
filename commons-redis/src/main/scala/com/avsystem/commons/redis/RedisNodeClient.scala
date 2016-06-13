package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.redis.Scope.Node
import com.avsystem.commons.redis.actor.{RedisConnectionPoolActor, RedisOperationActor}

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 05/04/16.
  */
final class RedisNodeClient(address: NodeAddress = NodeAddress.Default, poolSize: Int = 1)
  (implicit system: ActorSystem) extends Closeable {

  private val handlingActor = system.actorOf(Props(new RedisConnectionPoolActor(address, poolSize)))

  import system.dispatcher

  def executeOp[A](op: RedisOp[A])(implicit timeout: Timeout): Future[A] =
    handlingActor.ask(op).map {
      case RedisOperationActor.Response(result) => result.asInstanceOf[A]
      case RedisOperationActor.Failure(cause) => throw cause
    }

  //TODO possible optimized version for batches / LeafOps

  def toExecutor(implicit timeout: Timeout): RedisExecutor[Node] =
    new RedisExecutor[Node] {
      def execute[A](cmd: NodeBatch[A]) = executeOp(cmd.operation)
    }

  def close(): Unit =
    system.stop(handlingActor)
}
