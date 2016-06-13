package com.avsystem.commons
package redis

import akka.util.{ByteString, ByteStringBuilder, Timeout}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait CommandsSuite extends FunSuite with ScalaFutures with BeforeAndAfterAll {
  type Api <: ApiSubset

  def executor: RedisExecutor[Api#CmdScope]
  def setupCommands: RedisBatch[Any, Api#CmdScope] = RedisBatch.success(())
  val commands: Api {type Result[+A, -S] = Future[A]}

  implicit def executionContext: ExecutionContext

  implicit class ByteStringIterpolation(sc: StringContext) {
    def bs = this

    def apply(): ByteString = {
      val bsb = new ByteStringBuilder
      sc.parts.foreach(p => bsb.append(ByteString(p)))
      bsb.result()
    }
  }

  override protected def beforeAll() = {
    super.beforeAll()
    Await.result(executor.execute(setupCommands), Duration.Inf)
  }
}

trait RedisNodeCommandsSuite extends FunSuite with UsesRedisNodeClient with CommandsSuite {
  type Api = RedisNodeAsyncCommands
  implicit val timeout = Timeout(1.seconds)
  def executor = redisClient.toExecutor
  lazy val commands = RedisNodeAsyncCommands(executor)

  override protected def afterAll() = {
    Await.result(commands.flushall, Duration.Inf)
    super.afterAll()
  }
}

trait RedisConnectionCommandsSuite extends FunSuite with UsesRedisConnectionClient with CommandsSuite {
  type Api = RedisConnectionAsyncCommands
  implicit val timeout = Timeout(1.seconds)
  def executor = redisClient.toExecutor
  lazy val commands = RedisConnectionAsyncCommands(executor)

  override protected def afterAll() = {
    Await.result(commands.flushall, Duration.Inf)
    super.afterAll()
  }
}


