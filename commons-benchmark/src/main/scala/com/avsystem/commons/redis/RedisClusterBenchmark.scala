package com.avsystem.commons
package redis

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * Author: ghik
  * Created: 07/09/16.
  */
@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@Threads(64)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class RedisClusterBenchmark {

  import RedisClusterBenchmark._

  val key = ByteString("costam")
  implicit val system = ActorSystem("redis", ConfigFactory.defaultReference.withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("INFO")))
  var client: RedisClusterClient = _

  @Setup
  def setupClient(): Unit = {
    client = new RedisClusterClient(List(NodeAddress(port = 33330)))
    Await.result(client.initialized, Duration.Inf)
  }

  @TearDown
  def teardownClient(): Unit = {
    client.close()
    Await.result(system.terminate(), Duration.Inf)
  }

  @Benchmark
  @OperationsPerInvocation(SequencedFutures * BatchedGets)
  def redisClusterBenchmark(bh: Blackhole) = {
    def singleFut(i: Int) = client.executeBatch(RedisApi.Batches.BinaryTyped.get(key))(Timeout(30, TimeUnit.SECONDS))
    import com.avsystem.commons.concurrent.RunInQueueEC.Implicits.executionContext
    val resultFut = Future.traverse(0 until SequencedFutures: IndexedSeq[Int])(singleFut)
    Await.result(resultFut, Duration.Inf)
  }
}

object RedisClusterBenchmark {
  final val BatchedGets = 1
  final val SequencedFutures = 100
}
