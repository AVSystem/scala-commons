package com.avsystem.commons
package redis

import org.scalatest.Suite

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesRedisNodeClient extends UsesRedisServer with UsesActorSystem {this: Suite =>
  def poolSize = 5

  var redisClient: RedisNodeClient = _

  override protected def beforeAll() = {
    super.beforeAll()
    redisClient = new RedisNodeClient(address, poolSize)
  }

  override protected def afterAll() = {
    redisClient.close()
    super.afterAll()
  }
}
