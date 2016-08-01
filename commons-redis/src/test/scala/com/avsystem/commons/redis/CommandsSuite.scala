package com.avsystem.commons
package redis

import akka.util.{ByteString, ByteStringBuilder}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

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

trait CommandsSuite extends FunSuite with ScalaFutures with BeforeAndAfterAll with ByteStringInterpolation {
  type Api <: ApiSubset

  def executor: RedisExecutor[Api#CmdScope]
  def setupCommands: RedisBatch[Any, Api#CmdScope] = RedisBatch.success(())
  val commands: Api {type Result[+A, -S] = Future[A]}

  override protected def beforeAll() = {
    super.beforeAll()
    Await.result(executor.execute(setupCommands), Duration.Inf)
  }
}

trait RedisClusterCommandsSuite extends FunSuite with UsesPreconfiguredCluster with UsesRedisClusterClient with CommandsSuite {
  type Api = RedisClusteredAsyncCommands
  def executor = redisClient.toExecutor
  lazy val commands = RedisClusteredAsyncCommands(executor)

  override def clusterConfig = super.clusterConfig

  override protected def afterAll() = {
    // TODO: broadcast flushall?
    super.afterAll()
  }
}

trait RedisNodeCommandsSuite extends FunSuite with UsesRedisNodeClient with CommandsSuite {
  type Api = RedisNodeAsyncCommands
  def executor = redisClient.toExecutor
  lazy val commands = RedisNodeAsyncCommands(executor)

  override protected def afterAll() = {
    Await.result(commands.flushall, Duration.Inf)
    super.afterAll()
  }
}

trait RedisConnectionCommandsSuite extends FunSuite with UsesRedisConnectionClient with CommandsSuite {
  type Api = RedisConnectionAsyncCommands
  def executor = redisClient.toExecutor
  lazy val commands = RedisConnectionAsyncCommands(executor)

  override protected def afterAll() = {
    Await.result(commands.flushall, Duration.Inf)
    super.afterAll()
  }
}
