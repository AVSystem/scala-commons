package com.avsystem.commons
package redis

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.avsystem.commons.benchmark.CrossRedisBenchmark
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.jiop.Java8Interop._
import com.avsystem.commons.redis.RedisClientBenchmark._
import com.avsystem.commons.redis.actor.RedisConnectionActor.DebugListener
import com.avsystem.commons.redis.commands.SlotRange
import com.avsystem.commons.redis.config._
import com.typesafe.config._
import org.openjdk.jmh.annotations._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source

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
@Threads(4)
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

  val connectionConfig = ConnectionConfig(initCommands = clientSetname("benchmark"))
  val monConnectionConfig = ConnectionConfig(initCommands = clientSetname("benchmarkMon"))
  val nodeConfig = NodeConfig(poolSize = 4, connectionConfigs = _ => connectionConfig)
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

  val SlotKeys =
    Source.fromInputStream(getClass.getResourceAsStream("/slotkeys.txt"))
      .getLines().toArray

  final val BatchSize = 50
  final val ConcurrentCommands = 20000
  final val ConcurrentBatches = ConcurrentCommands / BatchSize
  final val ConcurrentOps = 2000

  val KeyBase = "key"
  val Value = "value"

  val Commands = Iterator.range(0, ConcurrentCommands).map(i => set(s"$KeyBase$i", Value)).toArray

  val OpSeqTL = new ThreadLocal[Int] {
    private val seq = new AtomicInteger(0)
    override def initialValue(): Int = seq.getAndIncrement()
  }
}

@OperationsPerInvocation(ConcurrentCommands)
class RedisClientBenchmark extends RedisBenchmark with CrossRedisBenchmark {

  import RedisApi.Batches.StringTyped._

  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  def seq = RedisClientBenchmark.OpSeqTL.get

  def commandFuture(client: RedisKeyedExecutor, i: Int) =
    client.executeBatch(set(s"$KeyBase$i", Value))

  def batchFuture(client: RedisKeyedExecutor, i: Int) = {
    val batch = (0 until BatchSize).map(j => set(s"{$KeyBase$i}$j", "v")).sequence
    client.executeBatch(batch)
  }

  def roundRobinBatchFuture(client: RedisClusterClient, i: Int) = {
    val mapping = client.currentState.mapping
    val (SlotRange(slot, _), nodeClient) = mapping(i % mapping.size)
    val batch = (0 until BatchSize).map(j => set(s"$KeyBase{${SlotKeys(slot)}}$i$j", "v")).sequence
    nodeClient.executeBatch(batch)
  }

  def distributedBatchFuture(client: RedisKeyedExecutor, i: Int) = {
    val batch = (0 to BatchSize).map(j => set(s"$KeyBase$i.$j", "v")).sequence
    client.executeBatch(batch)
  }

  def transactionFuture(client: RedisExecutor, i: Int) = {
    val batch = (0 until BatchSize).map(j => set(s"{$KeyBase$i}$j", "v")).sequence
    client.executeBatch(batch.transaction)
  }

  def clusterOperationFuture(client: RedisClusterClient, seq: Int, i: Int) = {
    val key = s"$KeyBase$seq.$i"
    val operation = for {
      value <- watch(key) *> get(key)
      _ <- set(key, value.getOrElse("v")).transaction
    } yield ()
    client.currentState.clientForSlot(keySlot(key)).executeOp(operation)
  }

  def operationFuture(client: RedisOpExecutor, seq: Int, i: Int) = {
    val key = s"$KeyBase$seq.$i"
    val operation = for {
      value <- watch(key) *> get(key)
      _ <- set(key, value.getOrElse("v")).transaction
    } yield ()
    client.executeOp(operation, ExecutionConfig(timeout = Timeout(2, TimeUnit.SECONDS)))
  }

  def mixedFuture(client: RedisKeyedExecutor with RedisOpExecutor, seq: Int, i: Int) =
    i % 3 match {
      case 0 => batchFuture(client, i)
      case 1 => transactionFuture(client, i)
      case 2 => operationFuture(client, seq, i)
    }

  def clusterMixedFuture(client: RedisClusterClient, seq: Int, i: Int) =
    i % 3 match {
      case 0 => distributedBatchFuture(client, i)
      case 1 => transactionFuture(client, i)
      case 2 => clusterOperationFuture(client, seq, i)
    }

  protected def redisClientBenchmark(futureCount: Int, singleFut: Int => Future[Any]) = {
    val start = System.nanoTime()
    val ctl = new CountDownLatch(futureCount)
    val failures = new ConcurrentHashMap[String, AtomicInteger]
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
      println(s"Took $millis$failuresRepr")
    }
  }

  @Benchmark
  def clusterClientCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, commandFuture(clusterClient, _))

  @Benchmark
  def clusterClientBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, batchFuture(clusterClient, _))

  @Benchmark
  def clusterClientRoundRobinBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, roundRobinBatchFuture(clusterClient, _))

  @Benchmark
  def clusterClientDistributedBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, distributedBatchFuture(clusterClient, _))

  @Benchmark
  def clusterClientTransactionBenchmark() =
    redisClientBenchmark(ConcurrentBatches, transactionFuture(clusterClient, _))

  @Benchmark
  @OperationsPerInvocation(ConcurrentOps)
  def clusterClientOperationBenchmark() =
    redisClientBenchmark(ConcurrentOps, clusterOperationFuture(clusterClient, seq, _))

  def clusterClientMixedBenchmark() =
    redisClientBenchmark(ConcurrentBatches, clusterMixedFuture(clusterClient, seq, _))

  @Benchmark
  def nodeClientCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, commandFuture(nodeClient, _))

  @Benchmark
  def nodeClientBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, batchFuture(nodeClient, _))

  @Benchmark
  def nodeClientTransactionBenchmark() =
    redisClientBenchmark(ConcurrentBatches, transactionFuture(nodeClient, _))

  def nodeClientMixedBenchmark() =
    redisClientBenchmark(ConcurrentBatches, mixedFuture(nodeClient, seq, _))

  @Benchmark
  @OperationsPerInvocation(ConcurrentOps)
  def nodeClientOperationBenchmark() =
    redisClientBenchmark(ConcurrentOps, operationFuture(nodeClient, seq, _))

  @Benchmark
  def connectionClientCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, commandFuture(connectionClient, _))

  @Benchmark
  def connectionClientBatchBenchmark() =
    redisClientBenchmark(ConcurrentBatches, batchFuture(connectionClient, _))

  @Benchmark
  def connectionClientTransactionBenchmark() =
    redisClientBenchmark(ConcurrentBatches, transactionFuture(connectionClient, _))

  @Benchmark
  @OperationsPerInvocation(ConcurrentOps)
  def connectionClientOperationBenchmark() =
    redisClientBenchmark(ConcurrentOps, operationFuture(connectionClient, seq, _))

  def connectionClientMixedBenchmark() =
    redisClientBenchmark(ConcurrentBatches, mixedFuture(connectionClient, seq, _))
}
