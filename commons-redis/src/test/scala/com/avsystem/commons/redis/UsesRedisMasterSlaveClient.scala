package com.avsystem.commons
package redis

import com.avsystem.commons.redis.config.MasterSlaveConfig
import org.scalatest.Suite

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesRedisMasterSlaveClient extends UsesMasterSlaveServers with UsesActorSystem { this: Suite =>
  def masterSlaveConfig: MasterSlaveConfig = MasterSlaveConfig()
  def seedSentinels: Seq[NodeAddress] = sentinelAddresses.take(1)

  var redisClient: RedisMasterSlaveClient = _

  def switchMaster(): Future[Unit] = {
    val client = new RedisConnectionClient(seedSentinels.head)
    val api = RedisApi.Connection.Async.StringTyped(client)
    val result = api.sentinelFailover(masterName)
    result.onCompleteNow(_ => client.close())
    result
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    redisClient = new RedisMasterSlaveClient(masterName, seedSentinels, masterSlaveConfig)
  }

  override protected def afterAll(): Unit = {
    redisClient.close()
    super.afterAll()
  }
}
