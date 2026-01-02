package com.avsystem.commons
package redis

import com.avsystem.commons.redis.config.{ConnectionConfig, NodeConfig}
import org.scalatest.Suite

/** Author: ghik Created: 14/04/16.
  */
trait UsesRedisNodeClient extends UsesRedisServer with UsesActorSystem with UsesSslContext { this: Suite =>
  def useTls: Boolean = false

  def connectionConfig: ConnectionConfig =
    ConnectionConfig(sslEngineCreator = if (useTls) OptArg(() => sslContext.createSSLEngine()) else OptArg.Empty)

  def nodeConfig: NodeConfig = NodeConfig(
    connectionConfigs = _ => connectionConfig,
    blockingConnectionConfigs = _ => connectionConfig,
  )

  var redisClient: RedisNodeClient = _

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    redisClient = new RedisNodeClient(if (useTls) tlsAddress else address, nodeConfig)
  }

  override protected def afterAll(): Unit = {
    redisClient.close()
    super.afterAll()
  }
}
