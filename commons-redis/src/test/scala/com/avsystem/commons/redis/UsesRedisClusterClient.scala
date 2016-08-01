package com.avsystem.commons
package redis

import com.avsystem.commons.redis.config.ClusterConfig
import org.scalatest.Suite

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesRedisClusterClient extends UsesClusterServers with UsesActorSystem {this: Suite =>
  def clusterConfig: ClusterConfig = ClusterConfig()

  var redisClient: RedisClusterClient = _

  override protected def beforeAll() = {
    super.beforeAll()
    redisClient = new RedisClusterClient(addresses.take(1), clusterConfig)
  }

  override protected def afterAll() = {
    redisClient.close()
    super.afterAll()
  }
}
