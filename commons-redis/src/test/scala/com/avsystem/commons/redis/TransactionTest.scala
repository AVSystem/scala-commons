package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.config.ConnectionConfig
import com.avsystem.commons.redis.exception.{ErrorReplyException, OptimisticLockException}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class TransactionTest extends RedisNodeCommandsSuite with CommunicationLogging {

  import RedisCommands._

  private val key = bs"key"

  override def nodeConfig = super.nodeConfig.copy(
    connectionConfigs = _ => ConnectionConfig(debugListener = listener)
  )

  override def setupCommands =
    super.setupCommands *> set(key, bs"42")

  test("empty transaction") {
    val batch = RedisBatch.success(42).transaction
    assert(redisClient.executeBatch(batch).futureValue == 42)

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
    val batch = set(bs"randomkey", bs"value").transaction
    assert(redisClient.executeBatch(batch).futureValue)

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
      get(bs"nestedkey"),
      set(bs"nestedkey", bs"value").transaction
      ).sequence.transaction

    assert(redisClient.executeBatch(batch).futureValue == (Opt.Empty, true))

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
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- set(key, value).transaction
    } yield value

    assert(redisClient.executeOp(operation).futureValue == bs"42")

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
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- RedisOp.success {
        // simulate concurrent client reading watched key
        val client = new RedisConnectionClient(redisClient.address)
        Await.result(client.execute(set(key, bs"42")), Duration.Inf)
        client.close()
      }
      _ <- set(key, value).transaction
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
}

class SingleConnectionTransactionTest extends RedisNodeCommandsSuite with CommunicationLogging {

  import RedisCommands._

  override def nodeConfig = super.nodeConfig.copy(
    poolSize = 1,
    connectionConfigs = _ => ConnectionConfig(debugListener = listener)
  )

  private val key = bs"key"

  override def setupCommands =
    super.setupCommands *> set(key, bs"0")

  // needed in order to force the client to execute UNWATCH before test finishes
  private def withDummyGet[T](fut: Future[T]) =
  fut.andThen({ case _ => redisClient.executeBatch(get(bs"dummy")) })

  test("simple transaction with cleanup") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- set(key, value)
    } yield value

    assert(withDummyGet(redisClient.executeOp(operation)).futureValue == bs"0")
    assert(listener.result().contains("UNWATCH"))
  }

  test("simple transaction with cleanup after failure") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- RedisOp.failure(new IllegalArgumentException("SRSLY"))
    } yield value

    intercept[IllegalArgumentException](throw withDummyGet(redisClient.executeOp(operation)).failed.futureValue)
    assert(listener.result().contains("UNWATCH"))
  }

  test("simple transaction with cleanup after redis failure") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- clusterInfo // cluster info will fail on non-cluster Redis instance
    } yield value

    intercept[ErrorReplyException](throw withDummyGet(redisClient.executeOp(operation)).failed.futureValue)
    assert(listener.result().contains("UNWATCH"))
  }

  test("concurrent transactions") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.map(_.utf8String.toInt).getOrElse(0))
      _ <- set(key, bs"${value + 1}").transaction
    } yield value

    def execute = redisClient.executeOp(operation)

    assert(Future.sequence(List.fill(3)(execute)).futureValue.toSet == Set(0, 1, 2))
  }
}
