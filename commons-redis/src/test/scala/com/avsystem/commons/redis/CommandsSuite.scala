package com.avsystem.commons
package redis

import akka.util.{ByteString, ByteStringBuilder}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterEach, FunSuite}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.reflect.ClassTag

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait ByteStringInterpolation {
  implicit class bsInterpolation(sc: StringContext) {
    def bs(args: Any*) = {
      val bsb = new ByteStringBuilder
      bsb.append(ByteString(sc.parts.head))
      (sc.parts.tail zip args.map(_.toString)).foreach {
        case (p, a) => bsb.append(ByteString(p)).append(ByteString(a))
      }
      bsb.result()
    }
  }
}

trait CommandsSuite extends FunSuite with ScalaFutures with BeforeAndAfterEach
  with ByteStringInterpolation with CommunicationLogging {

  val redisKey = bs"key"
  def executor: RedisExecutor

  protected def setup(batches: RedisBatch[Any]*): Unit = {
    Await.result(executor.execute(batches.sequence), Duration.Inf)
    listener.clear()
  }

  protected implicit class BatchOps[T](batch: RedisBatch[T]) {
    def exec: Future[T] = executor.execute(batch)
    def assert(pred: T => Boolean): Unit = CommandsSuite.this.assert(pred(exec.futureValue))
    def assertEquals(t: T): Unit = assert(_ == t)
    def intercept[E <: Throwable: Manifest]: E = CommandsSuite.this.intercept[E](throw exec.failed.futureValue)
  }
}

trait RedisClusterCommandsSuite extends FunSuite with UsesPreconfiguredCluster with UsesRedisClusterClient with CommandsSuite {
  def executor = redisClient.toExecutor

  override protected def afterEach() = {
    val futures = redisClient.currentState.masters.values.map(_.executeBatch(RedisCommands.flushall))
    Await.result(Future.sequence(futures), Duration.Inf)
    super.afterEach()
  }
}

trait RedisNodeCommandsSuite extends FunSuite with UsesRedisNodeClient with CommandsSuite {
  def executor = redisClient.toExecutor

  override protected def afterEach() = {
    Await.result(executor.execute(RedisCommands.flushall), Duration.Inf)
    super.afterEach()
  }
}

trait RedisConnectionCommandsSuite extends FunSuite with UsesRedisConnectionClient with CommandsSuite {
  def executor = redisClient.toExecutor

  override protected def afterEach() = {
    Await.result(executor.execute(RedisCommands.flushall), Duration.Inf)
    super.afterEach()
  }
}
