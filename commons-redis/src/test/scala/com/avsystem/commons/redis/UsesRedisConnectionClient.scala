package com.avsystem.commons
package redis

import com.avsystem.commons.redis.config.ConnectionConfig
import org.scalatest.Suite

/**
  * Author: ghik
  * Created: 09/06/16.
  */
trait UsesRedisConnectionClient extends UsesRedisServer with UsesActorSystem { this: Suite =>
  def connectionConfig: ConnectionConfig = ConnectionConfig()

  var redisClient: RedisConnectionClient = _

  override protected def beforeAll() = {
    super.beforeAll()
    redisClient = new RedisConnectionClient(address, connectionConfig)
  }

  override protected def afterAll() = {
    redisClient.close()
    super.afterAll()
  }
}
