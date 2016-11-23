package com.avsystem.commons
package redis

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.avsystem.commons.benchmark.CrossRedisBenchmark
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.redis.RedisClientBenchmark._
import com.avsystem.commons.redis.actor.RedisConnectionActor.DebugListener
import com.avsystem.commons.redis.config._
import com.typesafe.config._
import org.openjdk.jmh.annotations._

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object OutgoingTraffictStats extends DebugListener {
  val writeCount = new AtomicInteger
  val byteCount = new AtomicInteger

  def reset(): Unit = {
    writeCount.set(0)
    byteCount.set(0)
  }

  def onSend(data: ByteString) = {
    writeCount.incrementAndGet()
    byteCount.addAndGet(data.size)
  }
  def onReceive(data: ByteString) = ()
}

@Warmup(iterations = 20)
@Measurement(iterations = 40)
@Fork(1)
@Threads(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
abstract class RedisBenchmark {

  import RedisApi.Batches.StringTyped._

  val Config =
    """
      |
    """.stripMargin

  implicit val system = ActorSystem("redis",
    ConfigFactory.parseString(Config).withFallback(ConfigFactory.defaultReference()).resolve)

  val PoolSize = 4

  val connectionConfig = ConnectionConfig(initCommands = clientSetname("benchmark"))
  val monConnectionConfig = ConnectionConfig(initCommands = clientSetname("benchmarkMon"))
  val nodeConfig = NodeConfig(poolSize = PoolSize, connectionConfigs = _ => connectionConfig)
  val clusterConfig = ClusterConfig(nodeConfigs = _ => nodeConfig, monitoringConnectionConfigs = _ => monConnectionConfig)

  lazy val clusterClient = Await.result(new RedisClusterClient(List(NodeAddress(port = 33333)), clusterConfig).initialized, Duration.Inf)
  lazy val nodeClient = Await.result(new RedisNodeClient(config = nodeConfig).initialized, Duration.Inf)
  lazy val connectionClient = Await.result(new RedisConnectionClient(config = connectionConfig).initialized, Duration.Inf)

  @TearDown
  def teardownClient(): Unit = {
    Await.result(system.terminate(), Duration.Inf)
  }
}

object RedisClientBenchmark {

  import RedisApi.Batches.StringTyped._

  final val BatchSize = 50
  final val ConcurrentCommands = 20000
  final val ConcurrentBatches = ConcurrentCommands / BatchSize

  val KeyBase = "key"
  val Value = "value"

  val Commands = Iterator.range(0, ConcurrentCommands).map(i => set(s"$KeyBase$i", Value)).toArray
}

@OperationsPerInvocation(ConcurrentCommands)
class RedisClientBenchmark extends RedisBenchmark with CrossRedisBenchmark {

  import RedisApi.Batches.StringTyped._

  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  def commandFuture(client: RedisKeyedExecutor, i: Int) =
    client.executeBatch(set(s"$KeyBase$i", Value))

  def batchFuture(client: RedisKeyedExecutor, i: Int) = {
    val batch = (0 until BatchSize).map(j => set(s"{$KeyBase$i}$j", "v")).sequence
    client.executeBatch(batch)
  }

  def distributedBatchFuture(client: RedisKeyedExecutor, i: Int) = {
    val batch = (0 to BatchSize).map(j => set(s"$KeyBase$i.$j", "v")).sequence
    client.executeBatch(batch)
  }

  def operationFuture(client: RedisExecutor, i: Int) = {
    val batch = (0 until BatchSize).map(j => set(s"{$KeyBase$i}$j", "v")).sequence
    client.executeBatch(batch.transaction)
  }

  def clusterTransactionFuture(client: RedisClusterClient, i: Int) = {
    val key = s"$KeyBase$i"
    val transaction = (0 until BatchSize).map(j => set(s"{$KeyBase$i}$j", "v")).sequence.transaction
    val operation = for {
      _ <- watch(key)
      _ <- transaction
    } yield ()
    client.initialized.flatMapNow(_.currentState.clientForSlot(keySlot(key))
      .executeOp(operation))
  }

  def transactionFuture(client: RedisOpExecutor, i: Int) = {
    val key = s"$KeyBase$i"
    val transaction = (0 until BatchSize).map(j => set(s"{$KeyBase$i}$j", "v")).sequence.transaction
    val operation = for {
      _ <- watch(key)
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

  def clusterMixedFuture(client: RedisClusterClient, i: Int) =
    i % 3 match {
      case 0 => distributedBatchFuture(client, i)
      case 1 => operationFuture(client, i)
      case 2 => clusterTransactionFuture(client, i)
    }

  protected def redisClientBenchmark(futureCount: Int, singleFut: Int => Future[Any]) = {
    val start = System.nanoTime()
    val ctl = new CountDownLatch(futureCount)
    val failures = new ConcurrentHashMap[String, AtomicInteger]
    OutgoingTraffictStats.reset()
    (0 until futureCount).foreach { i =>
      singleFut(i).onComplete {
        case Success(_) =>
          ctl.countDown()
        case Failure(t) =>
          val cause = s"${t.getClass.getName}: ${t.getMessage}"
          failures.computeIfAbsent(cause)(_ => new AtomicInteger).incrementAndGet()
          ctl.countDown()
      }(RunNowEC)
    }
    ctl.await(60, TimeUnit.SECONDS)
    if (!failures.isEmpty) {
      val millis = (System.nanoTime() - start) / 1000000
      val failuresRepr = failures.asScala.opt.filter(_.nonEmpty)
        .map(_.iterator.map({ case (cause, count) => s"${count.get} x $cause" }).mkString(", failures:\n", "\n", "")).getOrElse("")
      import OutgoingTraffictStats._
      println(s"Took $millis$failuresRepr, ${writeCount.get} writes, ${byteCount.get / writeCount.get} bytes per write")
    }
  }

  @Benchmark
  def clusterClientCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, commandFuture(clusterClient, _))

  @Benchmark
  def clusterClientBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, batchFuture(clusterClient, _))

  @Benchmark
  def clusterClientDistributedBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, distributedBatchFuture(clusterClient, _))

  @Benchmark
  def clusterClientOpBenchmark() =
    redisClientBenchmark(ConcurrentBatches, operationFuture(clusterClient, _))

  @Benchmark
  def clusterClientTransactionBenchmark() =
    redisClientBenchmark(ConcurrentBatches, clusterTransactionFuture(clusterClient, _))

  @Benchmark
  def clusterClientMixedBenchmark() =
    redisClientBenchmark(ConcurrentBatches, clusterMixedFuture(clusterClient, _))

  @Benchmark
  def nodeClientCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, commandFuture(nodeClient, _))

  @Benchmark
  def nodeClientBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, batchFuture(nodeClient, _))

  @Benchmark
  def nodeClientOpBenchmark() =
    redisClientBenchmark(ConcurrentBatches, operationFuture(nodeClient, _))

  @Benchmark
  def nodeClientMixedBenchmark() =
    redisClientBenchmark(ConcurrentBatches, mixedFuture(nodeClient, _))

  @Benchmark
  def nodeClientTransactionBenchmark() =
    redisClientBenchmark(ConcurrentBatches, transactionFuture(nodeClient, _))

  @Benchmark
  def connectionClientCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, commandFuture(connectionClient, _))

  @Benchmark
  def connectionClientBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, batchFuture(connectionClient, _))

  @Benchmark
  def connectionClientOpBenchmark() =
    redisClientBenchmark(ConcurrentBatches, operationFuture(connectionClient, _))

  @Benchmark
  def connectionClientTransactionBenchmark() =
    redisClientBenchmark(ConcurrentBatches, transactionFuture(connectionClient, _))

  @Benchmark
  def connectionClientMixedBenchmark() =
    redisClientBenchmark(ConcurrentBatches, mixedFuture(connectionClient, _))
}
