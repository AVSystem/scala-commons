package com.avsystem.commons
package redis

import com.avsystem.commons.redis.config.NodeConfig
import org.scalatest.Suite

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesRedisNodeClient extends UsesRedisServer with UsesActorSystem {this: Suite =>
  def nodeConfig = NodeConfig()

  var redisClient: RedisNodeClient = _

  override protected def beforeAll() = {
    super.beforeAll()
    redisClient = new RedisNodeClient(address, nodeConfig)
  }

  override protected def afterAll() = {
    redisClient.close()
    super.afterAll()
  }
}
