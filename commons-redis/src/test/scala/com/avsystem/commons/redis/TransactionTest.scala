package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.config.ConnectionConfig
import com.avsystem.commons.redis.exception.{ErrorReplyException, OptimisticLockException}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class TransactionTest extends RedisNodeCommandsSuite with CommunicationLogging {

  import RedisApi.Batches.StringTyped._

  test("empty transaction") {
    setup(set("key", "42"))
    val batch = RedisBatch.success(42).transaction
    batch.assertEquals(42)

    assertCommunication(
      """
        |*1\r\n
        |$5\r\n
        |MULTI\r\n
        |*1\r\n
        |$4\r\n
        |EXEC\r\n
        |
        |+OK\r\n
        |*0\r\n
        | """.stripMargin
    )
  }

  test("simple transaction") {
    val batch = set("randomkey", "value").transaction
    batch.assert(identity)

    assertCommunication(
      """
        |*1\r\n
        |$5\r\n
        |MULTI\r\n
        |*3\r\n
        |$3\r\n
        |SET\r\n
        |$9\r\n
        |randomkey\r\n
        |$5\r\n
        |value\r\n
        |*1\r\n
        |$4\r\n
        |EXEC\r\n
        |
        |+OK\r\n
        |+QUEUED\r\n
        |*1\r\n
        |+OK\r\n
        | """.stripMargin
    )
  }

  test("nested transactions") {
    val batch = (
      get("nestedkey"),
      set("nestedkey", "value").transaction
    ).sequence.transaction

    batch.assertEquals((Opt.Empty, true))

    assertCommunication(
      """
        |*1\r\n
        |$5\r\n
        |MULTI\r\n
        |*2\r\n
        |$3\r\n
        |GET\r\n
        |$9\r\n
        |nestedkey\r\n
        |*3\r\n
        |$3\r\n
        |SET\r\n
        |$9\r\n
        |nestedkey\r\n
        |$5\r\n
        |value\r\n
        |*1\r\n
        |$4\r\n
        |EXEC\r\n
        |
        |+OK\r\n
        |+QUEUED\r\n
        |+QUEUED\r\n
        |*2\r\n
        |$-1\r\n
        |+OK\r\n
      """.stripMargin
    )
  }

  test("simple transaction with watch") {
    setup(set("key", "42"))

    val operation = for {
      value <- watch("key") *> get("key").map(_.getOrElse("0"))
      _ <- set("key", value).transaction
    } yield value

    assert(redisClient.executeOp(operation).futureValue == "42")

    assertCommunication(
      """
        |*2\r\n
        |$5\r\n
        |WATCH\r\n
        |$3\r\n
        |key\r\n
        |*2\r\n
        |$3\r\n
        |GET\r\n
        |$3\r\n
        |key\r\n
        |
        |+OK\r\n
        |$2\r\n
        |42\r\n
        |
        |*1\r\n
        |$5\r\n
        |MULTI\r\n
        |*3\r\n
        |$3\r\n
        |SET\r\n
        |$3\r\n
        |key\r\n
        |$2\r\n
        |42\r\n
        |*1\r\n
        |$4\r\n
        |EXEC\r\n
        |
        |+OK\r\n
        |+QUEUED\r\n
        |*1\r\n
        |+OK\r\n
        | """.stripMargin
    )
  }

  test("optimistic lock failure") {
    setup(set("key", "42"))

    val operation = for {
      value <- watch("key") *> get("key").map(_.getOrElse("0"))
      _ <- RedisOp.success {
        // simulate concurrent client reading watched key
        val client = new RedisConnectionClient(redisClient.address)
        Await.result(client.executeBatch(set("key", "42")), Duration.Inf)
        client.close()
      }
      _ <- set("key", value).transaction
    } yield value

    intercept[OptimisticLockException](throw redisClient.executeOp(operation).failed.futureValue)
    assertCommunication(
      """
        |*2\r\n
        |$5\r\n
        |WATCH\r\n
        |$3\r\n
        |key\r\n
        |*2\r\n
        |$3\r\n
        |GET\r\n
        |$3\r\n
        |key\r\n
        |
        |+OK\r\n
        |$2\r\n
        |42\r\n
        |
        |*1\r\n
        |$5\r\n
        |MULTI\r\n
        |*3\r\n
        |$3\r\n
        |SET\r\n
        |$3\r\n
        |key\r\n
        |$2\r\n
        |42\r\n
        |*1\r\n
        |$4\r\n
        |EXEC\r\n
        |
        |+OK\r\n
        |+QUEUED\r\n
        |*-1\r\n
      """.stripMargin
    )
  }

  test("EXECABORT") {
    val badCommand: RedisBatch[Unit] =
      new RedisUnitCommand with NodeCommand {
        val encoded = encoder("GET").result
      }

    val batch = (get("key").failed, badCommand.failed).sequence.transaction
    batch.exec.futureValue match {
      case (err1: ErrorReplyException, err2: ErrorReplyException) =>
        assert(err1.reply.errorCode == "EXECABORT")
        assert(err2.reply.errorCode == "ERR")
      case obj => throw new MatchError(obj)
    }

    assertCommunication(
      """
        |*1\r\n
        |$5\r\n
        |MULTI\r\n
        |*2\r\n
        |$3\r\n
        |GET\r\n
        |$3\r\n
        |key\r\n
        |*1\r\n
        |$3\r\n
        |GET\r\n
        |*1\r\n
        |$4\r\n
        |EXEC\r\n
        |
        |+OK\r\n
        |+QUEUED\r\n
        |-ERR wrong number of arguments for \'get\' command\r\n
        |-EXECABORT Transaction discarded because of previous errors.\r\n
      """.stripMargin
    )
  }
}

class SingleConnectionTransactionTest extends RedisNodeCommandsSuite {

  import RedisApi.Batches.StringTyped._

  override def nodeConfig = super.nodeConfig.copy(
    poolSize = 1,
    connectionConfigs = _ => ConnectionConfig(debugListener = listener)
  )

  test("simple transaction with cleanup") {
    setup(set("key", "0"))

    val operation = for {
      value <- watch("key") *> get("key").map(_.getOrElse("0"))
      _ <- set("key", value)
    } yield value

    assert(redisClient.executeOp(operation).futureValue == "0")
    ping.get // make sure UNWATCH got executed
    assert(listener.result().contains("UNWATCH"))
  }

  test("simple transaction with cleanup after failure") {
    setup(set("key", "0"))

    val operation = for {
      value <- watch("key") *> get("key").map(_.getOrElse("0"))
      _ <- RedisOp.failure(new IllegalArgumentException("SRSLY"))
    } yield value

    intercept[IllegalArgumentException](throw redisClient.executeOp(operation).failed.futureValue)
    ping.get // make sure UNWATCH got executed
    assert(listener.result().contains("UNWATCH"))
  }

  test("simple transaction with cleanup after redis failure") {
    setup(set("key", "0"))

    val operation = for {
      value <- watch("key") *> get("key").map(_.getOrElse("0"))
      _ <- clusterInfo // cluster info will fail on non-cluster Redis instance
    } yield value

    intercept[ErrorReplyException](throw redisClient.executeOp(operation).failed.futureValue)
    ping.get // make sure UNWATCH got executed
    assert(listener.result().contains("UNWATCH"))
  }

  test("concurrent transactions") {
    setup(set("key", "0"))

    val operation = for {
      value <- watch("key") *> get("key").map(_.map(_.toInt).getOrElse(0))
      _ <- set("key", s"${value + 1}").transaction
    } yield value

    def execute = redisClient.executeOp(operation)

    assert(Future.sequence(List.fill(3)(execute)).futureValue.toSet == Set(0, 1, 2))
  }
}
