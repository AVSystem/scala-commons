package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.config.{ClusterConfig, NodeConfig}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FunSuite, Matchers}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
class RedisClusterClientTest extends FunSuite with Matchers with ScalaFutures
  with UsesPreconfiguredCluster with UsesRedisClusterClient with ByteStringInterpolation {

  override def clusterConfig = ClusterConfig(nodeConfigs = addr => NodeConfig(poolSize = 1))

  test("simple get") {
    val commands = RedisClusteredAsyncCommands(redisClient.toExecutor)
    commands.get(bs"key").futureValue(PatienceConfig(Span(1, Seconds))) shouldBe Opt.Empty
  }

  test("distribution test") {
    val slots = List(
      0,
      7000,
      7001,
      1,
      14000,
      14001,
      7002
    )

    val batches = slots.map(s => RedisCommands.get(ClusterUtils.SlotKeys(s)))
    val batch = batches.sequence

    redisClient.executeBatch(batch).futureValue shouldBe slots.map(_ => Opt.Empty)
  }
}
