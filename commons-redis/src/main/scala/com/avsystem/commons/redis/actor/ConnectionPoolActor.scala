package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Props}
import com.avsystem.commons.redis.NodeAddress
import com.avsystem.commons.redis.actor.ConnectionPoolActor._
import com.avsystem.commons.redis.config.{ConnectionConfig, NodeConfig}
import com.typesafe.scalalogging.LazyLogging

class ConnectionPoolActor(address: NodeAddress, config: NodeConfig)
  extends Actor with LazyLogging {

  private val connections = new MArrayBuffer[ActorRef]

  def receive: Receive = {
    case CreateNewConnection if connections.size < config.maxBlockingPoolSize =>
      logger.info(s"Creating new blocking connection to $address")
      val connConfig: ConnectionConfig = config.blockingConnectionConfigs(connections.size)
      val props = Props(new RedisConnectionActor(address, connConfig))
      val connection = connConfig.actorName.fold(context.actorOf(props))(context.actorOf(props, _))
      connections += connection
      connection ! RedisConnectionActor.Open(mustInitiallyConnect = false, Promise[Unit]())
      sender() ! NewConnection(connection)
    case CreateNewConnection =>
      sender() ! Full
    case Close(cause) =>
      connections.foreach(_ ! RedisConnectionActor.Close(cause))
  }
}
object ConnectionPoolActor {
  object CreateNewConnection
  case class Close(cause: Throwable)

  case class NewConnection(connection: ActorRef)
  object Full
}
