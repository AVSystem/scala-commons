package com.avsystem.commons
package redis

import java.io.FileInputStream
import java.security.{KeyStore, SecureRandom}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.redis.RedisClientBenchmark._
import com.avsystem.commons.redis.actor.RedisConnectionActor.DebugListener
import com.avsystem.commons.redis.commands.SlotRange
import com.avsystem.commons.redis.config._
import com.typesafe.config._
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}
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

  def onSend(data: ByteString): Unit = {
    writeCount.incrementAndGet()
    byteCount.addAndGet(data.size)
  }

  def onReceive(data: ByteString): Unit = ()
}

@Warmup(iterations = 20)
@Measurement(iterations = 40)
@Fork(1)
@Threads(4)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
abstract class RedisBenchmark(useTls: Boolean) {

  import RedisApi.Batches.StringTyped._

  val Config: String =
    """
      |
    """.stripMargin

  implicit val system: ActorSystem = ActorSystem("redis",
    ConfigFactory.parseString(Config).withFallback(ConfigFactory.defaultReference()).resolve)

  lazy val sslContext: SSLContext = SSLContext.getInstance("TLSv1.2").setup { sslc =>
    val ks = KeyStore.getInstance("PKCS12")
    ks.load(new FileInputStream("../commons-redis/tls/redis.p12"), Array.empty)

    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    kmf.init(ks, Array.empty)

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(ks)

    sslc.init(kmf.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
  }

  val connectionConfig: ConnectionConfig = ConnectionConfig(
    sslEngineCreator = if(useTls) OptArg(() => sslContext.createSSLEngine()) else OptArg.Empty,
    initCommands = clientSetname("benchmark")
  )

  val monConnectionConfig: ConnectionConfig = ConnectionConfig(
    sslEngineCreator = if(useTls) OptArg(() => sslContext.createSSLEngine()) else OptArg.Empty,
    initCommands = clientSetname("benchmarkMon")
  )

  val address: NodeAddress =
    if(useTls) NodeAddress(port = 7379) else NodeAddress.Default

  val nodeConfig: NodeConfig = NodeConfig(
    poolSize = 4,
    connectionConfigs = _ => connectionConfig
  )

  val clusterConfig: ClusterConfig = ClusterConfig(
    nodeConfigs = _ => nodeConfig,
    monitoringConnectionConfigs = _ => monConnectionConfig
  )

  lazy val clusterClient: RedisClusterClient =
    Await.result(new RedisClusterClient(List(NodeAddress(port = 33333)), clusterConfig).initialized, Duration.Inf)

  lazy val nodeClient: RedisNodeClient =
    Await.result(new RedisNodeClient(address, nodeConfig).initialized, Duration.Inf)

  lazy val connectionClient: RedisConnectionClient =
    Await.result(new RedisConnectionClient(address, connectionConfig).initialized, Duration.Inf)

  @TearDown
  def teardownClient(): Unit = {
    Await.result(system.terminate(), Duration.Inf)
  }
}

object RedisClientBenchmark {

  import RedisApi.Batches.StringTyped._

  val SlotKeys: Array[String] =
    Source.fromInputStream(getClass.getResourceAsStream("/slotkeys.txt"))
      .getLines().toArray

  final val BatchSize = 50
  final val ConcurrentCommands = 20000
  final val ConcurrentBatches = ConcurrentCommands / BatchSize
  final val ConcurrentOps = 2000

  val KeyBase = "key"
  val Value = "value"

  val Commands: Array[RedisBatch[Boolean]] = Iterator.range(0, ConcurrentCommands).map(i => set(s"$KeyBase$i", Value)).toArray

  val OpSeqTL: ThreadLocal[Int] = new ThreadLocal[Int] {
    private val seq = new AtomicInteger(0)
    override def initialValue(): Int = seq.getAndIncrement()
  }
}

@OperationsPerInvocation(ConcurrentCommands)
abstract class AbstractRedisClientBenchmark(useTls: Boolean)
  extends RedisBenchmark(useTls) {

  import RedisApi.Batches.StringTyped._

  implicit val timeout: Timeout = Timeout(5, TimeUnit.SECONDS)

  def seq: Int = RedisClientBenchmark.OpSeqTL.get

  def commandFuture(client: RedisKeyedExecutor, i: Int): Future[Boolean] =
    client.executeBatch(set(s"$KeyBase$i", Value))

  def batchFuture(client: RedisKeyedExecutor, i: Int): Future[IIndexedSeq[Boolean]] = {
    val batch = (0 until BatchSize).map(j => set(s"{$KeyBase$i}$j", "v")).sequence
    client.executeBatch(batch)
  }

  def roundRobinBatchFuture(client: RedisClusterClient, i: Int): Future[IIndexedSeq[Boolean]] = {
    val mapping = client.currentState.mapping
    val (SlotRange(slot, _), nodeClient) = mapping(i % mapping.size)
    val batch = (0 until BatchSize).map(j => set(s"$KeyBase{${SlotKeys(slot)}}$i$j", "v")).sequence
    nodeClient.executeBatch(batch)
  }

  def distributedBatchFuture(client: RedisKeyedExecutor, i: Int): Future[IIndexedSeq[Boolean]] = {
    val batch = (0 to BatchSize).map(j => set(s"$KeyBase$i.$j", "v")).sequence
    client.executeBatch(batch)
  }

  def transactionFuture(client: RedisExecutor, i: Int): Future[IIndexedSeq[Boolean]] = {
    val batch = (0 until BatchSize).map(j => set(s"{$KeyBase$i}$j", "v")).sequence
    client.executeBatch(batch.transaction)
  }

  def clusterOperationFuture(client: RedisClusterClient, seq: Int, i: Int): Future[Unit] = {
    val key = s"$KeyBase$seq.$i"
    val operation = for {
      value <- watch(key) *> get(key)
      _ <- set(key, value.getOrElse("v")).transaction
    } yield ()
    client.currentState.clientForSlot(keySlot(key)).executeOp(operation)
  }

  def operationFuture(client: RedisOpExecutor, seq: Int, i: Int): Future[Unit] = {
    val key = s"$KeyBase$seq.$i"
    val operation = for {
      value <- watch(key) *> get(key)
      _ <- set(key, value.getOrElse("v")).transaction
    } yield ()
    client.executeOp(operation, ExecutionConfig(responseTimeout = Timeout(2, TimeUnit.SECONDS)))
  }

  def mixedFuture(client: RedisKeyedExecutor with RedisOpExecutor, seq: Int, i: Int): Future[Any] =
    i % 3 match {
      case 0 => batchFuture(client, i)
      case 1 => transactionFuture(client, i)
      case 2 => operationFuture(client, seq, i)
    }

  def clusterMixedFuture(client: RedisClusterClient, seq: Int, i: Int): Future[Any] =
    i % 3 match {
      case 0 => distributedBatchFuture(client, i)
      case 1 => transactionFuture(client, i)
      case 2 => clusterOperationFuture(client, seq, i)
    }

  protected def redisClientBenchmark(futureCount: Int, singleFut: Int => Future[Any]): Unit = {
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
  def clusterClientCommandBenchmark(): Unit =
    redisClientBenchmark(ConcurrentCommands, commandFuture(clusterClient, _))

  @Benchmark
  def clusterClientBatchBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, batchFuture(clusterClient, _))

  @Benchmark
  def clusterClientRoundRobinBatchBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, roundRobinBatchFuture(clusterClient, _))

  @Benchmark
  def clusterClientDistributedBatchBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, distributedBatchFuture(clusterClient, _))

  @Benchmark
  def clusterClientTransactionBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, transactionFuture(clusterClient, _))

  @Benchmark
  @OperationsPerInvocation(ConcurrentOps)
  def clusterClientOperationBenchmark(): Unit =
    redisClientBenchmark(ConcurrentOps, clusterOperationFuture(clusterClient, seq, _))

  def clusterClientMixedBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, clusterMixedFuture(clusterClient, seq, _))

  @Benchmark
  def nodeClientCommandBenchmark(): Unit =
    redisClientBenchmark(ConcurrentCommands, commandFuture(nodeClient, _))

  @Benchmark
  def nodeClientBatchBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, batchFuture(nodeClient, _))

  @Benchmark
  def nodeClientTransactionBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, transactionFuture(nodeClient, _))

  def nodeClientMixedBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, mixedFuture(nodeClient, seq, _))

  @Benchmark
  @OperationsPerInvocation(ConcurrentOps)
  def nodeClientOperationBenchmark(): Unit =
    redisClientBenchmark(ConcurrentOps, operationFuture(nodeClient, seq, _))

  @Benchmark
  def connectionClientCommandBenchmark(): Unit =
    redisClientBenchmark(ConcurrentCommands, commandFuture(connectionClient, _))

  @Benchmark
  def connectionClientBatchBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, batchFuture(connectionClient, _))

  @Benchmark
  def connectionClientTransactionBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, transactionFuture(connectionClient, _))

  @Benchmark
  @OperationsPerInvocation(ConcurrentOps)
  def connectionClientOperationBenchmark(): Unit =
    redisClientBenchmark(ConcurrentOps, operationFuture(connectionClient, seq, _))

  def connectionClientMixedBenchmark(): Unit =
    redisClientBenchmark(ConcurrentBatches, mixedFuture(connectionClient, seq, _))
}

class RedisClientBenchmark extends AbstractRedisClientBenchmark(useTls = false)
class RedisTlsClientBenchmark extends AbstractRedisClientBenchmark(useTls = true)