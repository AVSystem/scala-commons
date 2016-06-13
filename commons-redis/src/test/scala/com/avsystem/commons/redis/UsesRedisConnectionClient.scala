package com.avsystem.commons
package redis

import org.scalatest.Suite

/**
  * Author: ghik
  * Created: 09/06/16.
  */
trait UsesRedisConnectionClient extends UsesRedisServer with UsesActorSystem {this: Suite =>
  var redisClient: RedisConnectionClient = _

  override protected def beforeAll() = {
    super.beforeAll()
    redisClient = new RedisConnectionClient(address)
  }

  override protected def afterAll() = {
    redisClient.close()
    super.afterAll()
  }
}
