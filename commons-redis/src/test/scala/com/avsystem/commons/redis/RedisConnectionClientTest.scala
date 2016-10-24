package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.redis.config.ConnectionConfig
import com.avsystem.commons.redis.exception.ConnectionInitializationFailure
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
class RedisConnectionClientTest extends FunSuite
  with Matchers with ScalaFutures with UsesActorSystem with UsesRedisServer with ByteStringInterpolation {

  def createClient(initCommands: RedisBatch[Any]) =
    new RedisConnectionClient(address, config = ConnectionConfig(initCommands))

  test("client initialization test") {
    import RedisApi.Batches.StringTyped._
    val client = createClient(select(0) *> clientSetname("name") *> ping)

    val f1 = client.executeBatch(echo(bs"LOL1"))
    val f2 = client.executeBatch(echo(bs"LOL2"))
    val f3 = client.executeBatch(clientGetname)

    f1.futureValue shouldBe bs"LOL1"
    f2.futureValue shouldBe bs"LOL2"
    f3.futureValue shouldBe "name".opt
  }

  test("client initialization failure test") {
    import RedisApi.Batches.StringTyped._
    val client = createClient(clusterInfo)

    val f1 = client.executeBatch(echo(bs"LOL1"))
    val f2 = client.executeBatch(echo(bs"LOL2"))

    f1.failed.futureValue shouldBe a[ConnectionInitializationFailure]
    f2.failed.futureValue shouldBe a[ConnectionInitializationFailure]
  }

  test("api traits usage test") {
    val api = RedisApi.Connection.Async.StringTyped(createClient(RedisBatch.unit))

    val bvApi: api.WithValue[ByteString] =
      api.valueType[ByteString]

    api.set("key", "value").futureValue shouldEqual true
    bvApi.set("key", ByteString.empty).futureValue shouldEqual true
    api.keyType[ByteString].set(ByteString("key"), "value").futureValue shouldEqual true
    bvApi.keyType[ByteString].set(ByteString("key"), ByteString.empty).futureValue shouldEqual true
  }
}
