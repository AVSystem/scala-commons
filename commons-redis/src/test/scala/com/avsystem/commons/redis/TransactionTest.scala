package com.avsystem.commons
package redis

import com.avsystem.commons.redis.exception.{ErrorReplyException, RedisException}

/**
  * Author: ghik
  * Created: 10/06/16.
  */
class TransactionTest extends RedisNodeCommandsSuite {

  import RedisCommands._

  val key = bs"key"

  override def setupCommands =
    super.setupCommands *> set(key, bs"42")

  test("simple transaction") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- set(key, value).transaction
    } yield value

    assert(redisClient.executeOp(operation).futureValue == bs"42")
  }

  test("simple transaction with cleanup") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- set(key, value)
    } yield value

    assert(redisClient.executeOp(operation).futureValue == bs"42")
  }

  test("simple transaction with cleanup after failure") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- RedisOp.failure(new RedisException("SRSLY"))
    } yield value

    intercept[RedisException](throw redisClient.executeOp(operation).failed.futureValue)
  }

  test("simple transaction with cleanup after redis failure") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- clusterInfo // cluster info will fail on non-cluster Redis instance
    } yield value

    intercept[ErrorReplyException](throw redisClient.executeOp(operation).failed.futureValue)
  }
}

class SingleConnectionTransactionTest extends RedisNodeCommandsSuite {
  import RedisCommands._

  override def poolSize = 1

  val key = bs"key"

  override def setupCommands =
    super.setupCommands *> set(key, bs"42")

  test("simple concurrent transactions") {
    val operation = for {
      value <- watch(Seq(key)) *> get(key).map(_.getOrElse(bs"0"))
      _ <- set(key, value).transaction
    } yield value

    def execute = redisClient.executeOp(operation)
    val f1 = execute
    val f2 = execute
    val f3 = execute

    assert(f1.futureValue == bs"42")
    assert(f2.futureValue == bs"42")
    assert(f3.futureValue == bs"42")
  }

}