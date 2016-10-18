package com.avsystem.commons
package redis

import java.util.concurrent.{CountDownLatch, TimeUnit}

import akka.actor.ActorSystem
import akka.util.Timeout
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.redis.RedisClientBenchmark._
import com.avsystem.commons.redis.config.{ClusterConfig, NodeConfig}
import com.typesafe.config._
import org.openjdk.jmh.annotations._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

@Warmup(iterations = 20)
@Measurement(iterations = 20)
@Fork(1)
@Threads(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
abstract class RedisBenchmark {
  implicit val system = ActorSystem("redis", ConfigFactory.defaultReference.withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("INFO")))

  val nodeConfig = NodeConfig(poolSize = 8)
  val clusterConfig = ClusterConfig(nodeConfigs = _ => nodeConfig)

  lazy val clusterClient = new RedisClusterClient(List(NodeAddress(port = 33333)), clusterConfig)
  lazy val nodeClient = new RedisNodeClient(config = nodeConfig)
  lazy val connectionClient = new RedisConnectionClient

  @TearDown
  def teardownClient(): Unit = {
    Await.result(system.terminate(), Duration.Inf)
  }
}

object RedisClientBenchmark {
  final val BatchSize = 300
  final val SequencedFutures = 500
}

@OperationsPerInvocation(SequencedFutures * BatchSize)
class RedisClientBenchmark extends RedisBenchmark {

  import RedisApi.Batches.StringTyped._

  def batchFuture(client: RedisKeyedExecutor, i: Int) = {
    val batch = (0 to BatchSize).map(_ => set(s"key$i", "v")).sequence
    client.executeBatch(batch)(Timeout(2, TimeUnit.SECONDS))
  }

  def distributedBatchFuture(client: RedisKeyedExecutor, i: Int) = {
    val batch = (0 to BatchSize).map(j => set(s"key$i.$j", "v")).sequence
    client.executeBatch(batch)(Timeout(2, TimeUnit.SECONDS))
  }

  def operationFuture(client: RedisOpExecutor, i: Int) = {
    val batch = (0 to BatchSize).map(_ => set(s"key$i", "v")).sequence
    client.executeOp(batch.operation)(Timeout(2, TimeUnit.SECONDS))
  }

  def transactionFuture(client: RedisOpExecutor, i: Int) = {
    val keys = (0 to BatchSize).map(_ => s"key$i")
    val transaction = keys.map(set(_, "v")).sequence.transaction
    val operation = for {
      _ <- watch(s"key$i")
      _ <- transaction
    } yield ()
    client.executeOp(operation)(Timeout(2, TimeUnit.SECONDS))
  }

  def mixedFuture(client: RedisKeyedExecutor with RedisOpExecutor, i: Int) =
    i % 3 match {
      case 0 => batchFuture(client, i)
      case 1 => operationFuture(client, i)
      case 2 => transactionFuture(client, i)
    }

  private def redisClientBenchmark(singleFut: Int => Future[Any]) = {
    val ctl = new CountDownLatch(SequencedFutures)
    (0 until SequencedFutures).foreach { i =>
      singleFut(i).onComplete {
        case Success(_) =>
          ctl.countDown()
        case Failure(t) =>
          ctl.countDown()
          t.printStackTrace()
      }(RunNowEC)
    }
    ctl.await(3, TimeUnit.SECONDS)
  }

  @Benchmark
  def clusterClientBatchBenchmark() = redisClientBenchmark(batchFuture(clusterClient, _))

  @Benchmark
  def clusterClientDistributedBatchBenchmark() = redisClientBenchmark(distributedBatchFuture(clusterClient, _))

  @Benchmark
  def nodeClientBatchBenchmark() = redisClientBenchmark(batchFuture(nodeClient, _))

  @Benchmark
  def connectionClientBatchBenchmark() = redisClientBenchmark(batchFuture(connectionClient, _))

  @Benchmark
  def nodeClientOpBenchmark() = redisClientBenchmark(operationFuture(nodeClient, _))

  @Benchmark
  def connectionClientOpBenchmark() = redisClientBenchmark(operationFuture(connectionClient, _))

  @Benchmark
  def nodeClientTransactionBenchmark() = redisClientBenchmark(transactionFuture(nodeClient, _))

  @Benchmark
  def connectionClientTransactionBenchmark() = redisClientBenchmark(transactionFuture(connectionClient, _))

  @Benchmark
  def nodeClientMixedBenchmark() = redisClientBenchmark(mixedFuture(nodeClient, _))

  @Benchmark
  def connectionClientMixedBenchmark() = redisClientBenchmark(mixedFuture(connectionClient, _))
}
