package com.avsystem.commons
package redis.commands

import akka.util.{ByteString, ByteStringBuilder, Timeout}
import com.avsystem.commons.redis.{RedisBatch, UsesRedisNodeClient}
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait CommandsSuite extends FunSuite with ScalaFutures {
  def execute[A](cmd: RedisBatch[A]): Future[A]
  def setupCommands: RedisBatch[Any] = RedisBatch.success(())

  implicit def executionContext: ExecutionContext

  implicit class ByteStringIterpolation(sc: StringContext) {
    def bs = this

    def apply(): ByteString = {
      val bsb = new ByteStringBuilder
      sc.parts.foreach(p => bsb.append(ByteString(p)))
      bsb.result()
    }
  }

  val commands = RedisCommands.transform(
    new PolyFun[RedisBatch, Future] {
      def apply[A](fa: RedisBatch[A]) = execute(fa)
    })
}

trait RedisNodeCommandsSuite extends CommandsSuite with UsesRedisNodeClient {
  implicit val timeout = Timeout(1.seconds)

  def execute[A](cmd: RedisBatch[A]) = redisClient.execute(cmd.operation)

  override protected def beforeAll() = {
    super.beforeAll()
    Await.result(execute(setupCommands), Duration.Inf)
  }

  override protected def afterAll() = {
    // TODO flushall here
    super.afterAll()
  }
}
