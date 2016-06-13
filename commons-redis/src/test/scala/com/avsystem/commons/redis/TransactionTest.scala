package com.avsystem.commons
package redis

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
}
