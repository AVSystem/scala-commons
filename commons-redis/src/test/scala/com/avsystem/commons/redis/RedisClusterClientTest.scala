package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
class RedisClusterClientTest extends FunSuite with Matchers with ScalaFutures
  with UsesPreconfiguredCluster with UsesRedisClusterClient with ByteStringInterpolation {

  test("simple get") {
    val commands = RedisClusteredAsyncCommands(redisClient.toAtomicExecutor)
    Thread.sleep(60000)
    commands.get(bs"key").futureValue shouldBe Opt.Empty
  }
}
