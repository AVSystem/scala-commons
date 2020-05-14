package com.avsystem.commons
package redis

import akka.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.misc.SourceInfo
import com.avsystem.commons.redis.config._
import org.scalactic.source.Position
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait ByteStringInterpolation {
  implicit class bsInterpolation(sc: StringContext) {
    def bs(args: Any*): ByteString = {
      val bsb = new ByteStringBuilder
      bsb.append(ByteString(sc.parts.head))
      (sc.parts.tail zip args).foreach {
        case (p, a: ByteString) => bsb.append(a).append(ByteString(p))
        case (p, a) => bsb.append(ByteString(String.valueOf(a))).append(ByteString(p))
      }
      bsb.result()
    }

    def bin(args: Any*): ByteString = {
      def extractByte(chars: Iterator[Char], bits: Int = 8): Byte = {
        var b = 0
        var c = 0
        while (c < bits) {
          chars.next() match {
            case '0' => b = b << 1
            case '1' => b = (b << 1) + 1
            case _ => throw new IllegalArgumentException("binary digit expected")
          }
          c += 1
        }
        b.toByte
      }
      val str = sc.s(args: _*)
      val firstBits = str.length % 8
      val bsb = new ByteStringBuilder
      val it = str.iterator
      if (firstBits != 0) {
        bsb += extractByte(it, firstBits)
      }
      while (it.hasNext) {
        bsb += extractByte(it)
      }
      bsb.result()
    }
  }
}

trait CommandsSuite extends AnyFunSuite with ScalaFutures with Matchers with UsesActorSystem
  with ByteStringInterpolation with CommunicationLogging {

  def executor: RedisKeyedExecutor
  def executionConfig: ExecutionConfig = ExecutionConfig.Default

  protected lazy val asyncKeyedCommands: RedisApi.Keyed.Async.StringTyped =
    RedisApi.Keyed.Async.StringTyped(executor)

  protected def setup(batches: RedisBatch[Any]*): Unit = {
    Await.result(executor.executeBatch(batches.sequence, executionConfig), Duration.Inf)
    listener.clear()
  }

  protected implicit class BatchOps[T](batch: RedisBatch[T]) {
    def get: T = Await.result(exec, patienceConfig.timeout.totalNanos.nanos)
    def exec: Future[T] = executor.executeBatch(batch, executionConfig)
    def assert(pred: T => Boolean)(implicit pos: Position): Unit = CommandsSuite.this.assert(pred(get))
    def assertEquals(t: T)(implicit pos: Position): Unit = assertResult(t)(get)
    def intercept[E <: Throwable : ClassTag](implicit pos: Position): E = CommandsSuite.this.intercept[E](get)
  }

  protected def cleanupBatch: RedisBatch[Any] =
    RedisApi.Batches.StringTyped.scriptFlush *> RedisApi.Batches.StringTyped.flushall

  protected def apiTest(testName: String)(testFun: => Any)(implicit sourceInfo: SourceInfo): Unit = {
    val suiteName = sourceInfo.enclosingSymbols.find(_.endsWith("ApiSuite")).map(_.stripSuffix("ApiSuite")).get
    test(testName, Tag(suiteName))(testFun)(Position(sourceInfo.fileName, sourceInfo.filePath, sourceInfo.line))
  }
}

abstract class RedisClusterCommandsSuite extends AnyFunSuite with UsesPreconfiguredCluster with UsesRedisClusterClient with CommandsSuite {
  def executor: RedisKeyedExecutor = redisClient

  override def clusterConfig: ClusterConfig =
    super.clusterConfig |> { cc =>
      cc.copy(
        nodeConfigs = a => cc.nodeConfigs(a) |> { nc =>
          nc.copy(
            poolSize = 4,
            connectionConfigs = i =>
              nc.connectionConfigs(i).copy(debugListener = listener)
          )
        }
      )
    }

  override protected def afterEach(): Unit = {
    val futures = redisClient.currentState.masters.values.map(_.executeBatch(cleanupBatch))
    Await.ready(Future.sequence(futures), Duration.Inf)
    super.afterEach()
  }
}

abstract class RedisMasterSlaveCommandsSuite
  extends AnyFunSuite with UsesPreconfiguredMasterSlave with UsesRedisMasterSlaveClient with CommandsSuite {

  def executor: RedisKeyedExecutor = redisClient

  override def masterSlaveConfig: MasterSlaveConfig =
    super.masterSlaveConfig |> { msc =>
      msc.copy(
        masterConfig = a => msc.masterConfig(a) |> { mc =>
          mc.copy(
            poolSize = 4,
            connectionConfigs = i =>
              mc.connectionConfigs(i).copy(debugListener = listener)
          )
        }
      )
    }

  override protected def afterEach(): Unit = {
    val futures = redisClient.currentMaster.toList.map(_.executeBatch(cleanupBatch))
    Await.ready(Future.sequence(futures), Duration.Inf)
    super.afterEach()
  }
}

abstract class RedisNodeCommandsSuite extends AnyFunSuite with UsesRedisNodeClient with CommandsSuite {
  def executor: RedisNodeClient = redisClient

  override def nodeConfig: NodeConfig =
    super.nodeConfig |> { nc =>
      nc.copy(
        poolSize = 4,
        connectionConfigs = i =>
          nc.connectionConfigs(i).copy(debugListener = listener)
      )
    }

  override protected def afterEach(): Unit = {
    Await.ready(executor.executeBatch(cleanupBatch), Duration.Inf)
    super.afterEach()
  }
}

abstract class RedisConnectionCommandsSuite extends AnyFunSuite with UsesRedisConnectionClient with CommandsSuite {
  def executor: RedisConnectionClient = redisClient

  override def connectionConfig: ConnectionConfig =
    super.connectionConfig.copy(debugListener = listener)

  override protected def afterEach(): Unit = {
    Await.ready(executor.executeBatch(cleanupBatch), Duration.Inf)
    super.afterEach()
  }
}
