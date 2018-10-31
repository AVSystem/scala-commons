package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.redis.config.{ConnectionConfig, NodeConfig}
import com.avsystem.commons.redis.exception.{ConnectionFailedException, ConnectionInitializationFailure, NodeInitializationFailure, TooManyConnectionsException}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
class RedisNodeClientTest extends FunSuite
  with Matchers with ScalaFutures with UsesActorSystem with UsesRedisServer with ByteStringInterpolation {

  def createClient(connInitCommands: RedisBatch[Any], initOp: RedisOp[Any]) =
    new RedisNodeClient(address, config = NodeConfig(
      initOp = initOp,
      maxBlockingPoolSize = 100,
      connectionConfigs = _ => ConnectionConfig(connInitCommands),
      blockingConnectionConfigs = _ => ConnectionConfig(connInitCommands)
    ))

  test("client initialization test") {
    import RedisApi.Batches.StringTyped._
    val client = createClient(select(0) *> ping, ping.operation)

    val f1 = client.executeBatch(echo(ByteString("LOL1")))
    val f2 = client.executeBatch(echo(ByteString("LOL2")))

    client.initialized.futureValue shouldBe client
    f1.futureValue shouldBe ByteString("LOL1")
    f2.futureValue shouldBe ByteString("LOL2")
  }

  test("client connection failure test") {
    import RedisApi.Batches.StringTyped._
    val client = new RedisConnectionClient(NodeAddress(port = 63498))

    val f1 = client.executeBatch(echo(ByteString("LOL1")))
    val f2 = client.executeBatch(echo(ByteString("LOL2")))

    client.initialized.failed.futureValue shouldBe a[ConnectionFailedException]
    f1.failed.futureValue shouldBe a[ConnectionFailedException]
    f2.failed.futureValue shouldBe a[ConnectionFailedException]
  }

  test("connection initialization failure test") {
    import RedisApi.Batches.StringTyped._
    val client = createClient(clusterInfo, RedisOp.unit)

    val f1 = client.executeBatch(echo(ByteString("LOL1")))
    val f2 = client.executeBatch(echo(ByteString("LOL2")))

    client.initialized.failed.futureValue shouldBe a[ConnectionInitializationFailure]
    f1.failed.futureValue shouldBe a[ConnectionInitializationFailure]
    f2.failed.futureValue shouldBe a[ConnectionInitializationFailure]
  }

  test("client initialization failure test") {
    import RedisApi.Batches.StringTyped._
    val client = createClient(RedisBatch.unit, clusterInfo.operation)

    val f1 = client.executeBatch(echo(ByteString("LOL1")))
    val f2 = client.executeBatch(echo(ByteString("LOL2")))

    client.initialized.failed.futureValue shouldBe a[NodeInitializationFailure]
    f1.failed.futureValue shouldBe a[NodeInitializationFailure]
    f2.failed.futureValue shouldBe a[NodeInitializationFailure]
  }

  test("concurrent blocking commands test") {
    val client = createClient(RedisBatch.unit, RedisOp.unit)
    val api = RedisApi.Node.Async.StringTyped(client)
    def fut: Future[Seq[Opt[String]]] = Future.sequence(Seq.fill(100)(api.blpop("LOL", 1)))
    fut.futureValue shouldBe Seq.fill(100)(Opt.Empty)
    fut.futureValue shouldBe Seq.fill(100)(Opt.Empty)
  }

  test("too many concurrent blocking commands test") {
    val client = createClient(RedisBatch.unit, RedisOp.unit).initialized.futureValue
    val api = RedisApi.Node.Async.StringTyped(client)
    val fut = Future.sequence(Seq.fill(100)(api.blpop("LOL", 1)))
    val failingFut = api.blpop("LOL", 1)
    fut.futureValue shouldBe Seq.fill(100)(Opt.Empty)
    failingFut.failed.futureValue shouldBe a[TooManyConnectionsException]
  }
}
