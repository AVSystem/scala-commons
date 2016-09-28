package com.avsystem.commons
package redis

import akka.util.{ByteString, ByteStringBuilder}
import org.scalactic.source.Position
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
      (sc.parts.tail zip args).foreach {
        case (p, a: ByteString) => bsb.append(a).append(ByteString(p))
        case (p, a) => bsb.append(ByteString(String.valueOf(a))).append(ByteString(p))
      }
      bsb.result()
    }
  }
}

trait CommandsSuite extends FunSuite with ScalaFutures with BeforeAndAfterEach
  with ByteStringInterpolation with CommunicationLogging {

  val redisKey = "key"
  def executor: RedisExecutor

  protected def setup(batches: RedisBatch[Any]*): Unit = {
    Await.result(executor.execute(batches.sequence), Duration.Inf)
    listener.clear()
  }

  protected implicit class BatchOps[T](batch: RedisBatch[T]) {
    def get(implicit pos: Position): T = exec.futureValue
    def exec: Future[T] = executor.execute(batch)
    def assert(pred: T => Boolean)(implicit pos: Position): Unit = CommandsSuite.this.assert(pred(get))
    def assertEquals(t: T)(implicit pos: Position): Unit = assertResult(t)(get)
    def intercept[E <: Throwable : ClassTag](implicit pos: Position): E = CommandsSuite.this.intercept[E](throw exec.failed.futureValue)
  }
}

abstract class RedisClusterCommandsSuite extends FunSuite with UsesPreconfiguredCluster with UsesRedisClusterClient with CommandsSuite {
  def executor = redisClient.toExecutor

  override def clusterConfig =
    super.clusterConfig |> { cc =>
      cc.copy(
        nodeConfigs = a => cc.nodeConfigs(a) |> { nc =>
          nc.copy(
            connectionConfigs = i =>
              nc.connectionConfigs(i) |> { mcc =>
                mcc.copy(connectionConfig = mcc.connectionConfig.copy(debugListener = listener))
              }
          )
        }
      )
    }

  override protected def afterEach() = {
    val futures = redisClient.currentState.masters.values.map(_.executeBatch(RedisBinaryCommands.flushall))
    Await.ready(Future.sequence(futures), Duration.Inf)
    super.afterEach()
  }
}

abstract class RedisNodeCommandsSuite extends FunSuite with UsesRedisNodeClient with CommandsSuite {
  def executor = redisClient.toExecutor

  override def nodeConfig =
    super.nodeConfig |> { nc =>
      nc.copy(
        poolSize = 1,
        connectionConfigs = i =>
          nc.connectionConfigs(i) |> { mcc =>
            mcc.copy(connectionConfig = mcc.connectionConfig.copy(debugListener = listener))
          }
      )
    }

  override protected def afterEach() = {
    Await.ready(executor.execute(RedisBinaryCommands.flushall), Duration.Inf)
    super.afterEach()
  }
}

abstract class RedisConnectionCommandsSuite extends FunSuite with UsesRedisConnectionClient with CommandsSuite {
  def executor = redisClient.toExecutor

  override def connectionConfig =
    super.connectionConfig.copy(debugListener = listener)

  override protected def afterEach() = {
    Await.ready(executor.execute(RedisBinaryCommands.flushall), Duration.Inf)
    super.afterEach()
  }
}
