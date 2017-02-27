package com.avsystem.commons
package redis

import com.avsystem.commons.redis.config.{ConnectionConfig, NodeConfig}
import com.avsystem.commons.redis.exception.{ConnectionFailedException, ConnectionInitializationFailure, NodeInitializationFailure}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
class RedisNodeClientTest extends FunSuite
  with Matchers with ScalaFutures with UsesActorSystem with UsesRedisServer with ByteStringInterpolation {

  def createClient(connInitCommands: RedisBatch[Any], initOp: RedisOp[Any]) =
    new RedisNodeClient(address, config = NodeConfig(initOp = initOp, connectionConfigs = _ => ConnectionConfig(connInitCommands)))

  test("client initialization test") {
    import RedisApi.Batches.StringTyped._
    val client = createClient(select(0) *> ping, ping.operation)

    val f1 = client.executeBatch(echo("LOL1"))
    val f2 = client.executeBatch(echo("LOL2"))

    client.initialized.futureValue shouldBe client
    f1.futureValue shouldBe "LOL1"
    f2.futureValue shouldBe "LOL2"
  }

  test("client connection failure test") {
    import RedisApi.Batches.StringTyped._
    val client = new RedisConnectionClient(NodeAddress(port = 63498))

    val f1 = client.executeBatch(echo("LOL1"))
    val f2 = client.executeBatch(echo("LOL2"))

    client.initialized.failed.futureValue shouldBe a[ConnectionFailedException]
    f1.failed.futureValue shouldBe a[ConnectionFailedException]
    f2.failed.futureValue shouldBe a[ConnectionFailedException]
  }

  test("connection initialization failure test") {
    import RedisApi.Batches.StringTyped._
    val client = createClient(clusterInfo, RedisOp.unit)

    val f1 = client.executeBatch(echo("LOL1"))
    val f2 = client.executeBatch(echo("LOL2"))

    client.initialized.failed.futureValue shouldBe a[ConnectionInitializationFailure]
    f1.failed.futureValue shouldBe a[ConnectionInitializationFailure]
    f2.failed.futureValue shouldBe a[ConnectionInitializationFailure]
  }

  test("client initialization failure test") {
    import RedisApi.Batches.StringTyped._
    val client = createClient(RedisBatch.unit, clusterInfo.operation)

    val f1 = client.executeBatch(echo("LOL1"))
    val f2 = client.executeBatch(echo("LOL2"))

    client.initialized.failed.futureValue shouldBe a[NodeInitializationFailure]
    f1.failed.futureValue shouldBe a[NodeInitializationFailure]
    f2.failed.futureValue shouldBe a[NodeInitializationFailure]
  }
}
