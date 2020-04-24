package com.avsystem.commons
package redis

import com.avsystem.commons.redis.exception.MasterSlaveInitializationException
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RedisMasterSlaveClientInitTest extends AnyFunSuite
  with Matchers with ScalaFutures with UsesActorSystem with UsesPreconfiguredMasterSlave {

  import RedisApi.Batches.StringTyped._

  def createClient(sentinelPorts: Int*): RedisMasterSlaveClient =
    new RedisMasterSlaveClient(masterName, sentinelPorts.map(p => NodeAddress(port = p)))

  test("client init test") {
    val client = createClient(sentinelPorts.head)
    client.initialized.futureValue shouldBe client
    client.executeBatch(get("key")).futureValue shouldBe Opt.Empty
  }

  test("not a sentinel") {
    val client = createClient(ports.head)
    client.initialized.failed.futureValue shouldBe a[MasterSlaveInitializationException]
    client.executeBatch(get("lol")).failed.futureValue shouldBe a[MasterSlaveInitializationException]
  }

  test("wrong master name") {
    val client = new RedisMasterSlaveClient("wrongmaster", sentinelPorts.map(p => NodeAddress(port = p)))
    client.initialized.failed.futureValue shouldBe a[MasterSlaveInitializationException]
    client.executeBatch(get("lol")).failed.futureValue shouldBe a[MasterSlaveInitializationException]
  }
}

class RedisMasterSlaveFailoverTest extends RedisMasterSlaveCommandsSuite {

  import RedisApi.Batches.StringTyped._

  test("failover test") {
    val failoverPromise = Promise[Unit]
    redisClient.setMasterListener { client =>
      if (client.address.port != ports.head) {
        failoverPromise.success(())
      }
    }

    val getset = set("key", "walue") *> get("key")

    getset.assertEquals(Opt("walue"))
    val smFut = switchMaster()
    getset.assertEquals(Opt("walue"))
    smFut.futureValue
    getset.assertEquals(Opt("walue"))
    failoverPromise.future.futureValue // wait on new master
    getset.assertEquals(Opt("walue"))
  }
}
