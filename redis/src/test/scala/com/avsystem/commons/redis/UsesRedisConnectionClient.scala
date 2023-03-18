package com.avsystem.commons
package redis

import com.avsystem.commons.redis.config.ConnectionConfig
import org.scalatest.Suite

/**
  * Author: ghik
  * Created: 09/06/16.
  */
trait UsesRedisConnectionClient extends UsesRedisServer with UsesActorSystem with UsesSslContext { this: Suite =>
  def useTls: Boolean = false

  def connectionConfig: ConnectionConfig =
    ConnectionConfig(sslEngineCreator = if (useTls) OptArg(() => sslContext.createSSLEngine()) else OptArg.Empty)

  var redisClient: RedisConnectionClient = _

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    redisClient = new RedisConnectionClient(if (useTls) tlsAddress else address, connectionConfig)
  }

  override protected def afterAll(): Unit = {
    redisClient.close()
    super.afterAll()
  }
}
