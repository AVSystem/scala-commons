package com.avsystem.commons
package redis.examples

import akka.actor.ActorSystem
import akka.util.Timeout
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.config._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Examples showing how to configure various client implementations so that connections to Redis servers
  * are properly initialized, e.g. authentication is performed.
  */
object ConnectionSetupExample extends App {
  implicit val actorSystem = ActorSystem()

  // In order to authenticate to Redis, every connection must send an AUTH command upon initializing.
  // We can specify "initialization commands" for a connection in ConnectionConfig:
  val authConfig = ConnectionConfig(
    initCommands = RedisApi.Batches.StringTyped.auth("mypassword")
  )

  val api = RedisApi.Batches.StringTyped

  // Apart from authentication, initialization commands can also be used for other connection setup operations,
  // e.g. choosing database with SELECT command
  // One way to group multiple commands into a single batch is to use *> operator
  val authSelectConfig = ConnectionConfig(
    initCommands = api.auth("mypassword") *> api.select(2)
  )

  // RedisConnectionClient accepts ConnectionConfig directly, as constructor parameter
  val connectionClient = new RedisConnectionClient(config = authConfig)
  // RedisNodeClient uses connection pool and may accept different configuration for every connection
  val nodeClient = new RedisNodeClient(
    config = NodeConfig(connectionConfigs = _ => authConfig)
  )
  // Cluster client connects to multiple nodes and may accept different configuration for every node
  // thus adding yet another level of configuration nesting.
  // We also need to provide separate config for monitoring connections.
  val clusterClient = new RedisClusterClient(
    config = ClusterConfig(
      nodeConfigs = _ => NodeConfig(connectionConfigs = _ => authConfig),
      monitoringConnectionConfigs = _ => authConfig
    )
  )

  // On connection client it's possible to execute AUTH, SELECT and similar commands directly:
  val connApi = RedisApi.Connection.Async.StringTyped(connectionClient)
  val initFuture: Future[Unit] =
    for {
      _ <- connApi.auth("mypassword")
      _ <- connApi.select(2)
    } yield ()
}
