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
  @OperationsPerInvocation(RedisClusterBenchmark.SequencedFutures)
  def redisClusterBenchmark(bh: Blackhole): Unit = {
    def singleFut = client.executeBatch(RedisCommands.get(ByteString(s"costam")))(Timeout(5, TimeUnit.SECONDS))
    import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext
    val resultFut = Future.traverse(0 until RedisClusterBenchmark.SequencedFutures: IndexedSeq[Int])(_ => singleFut)
    bh.consume(Await.result(resultFut, Duration.Inf))
  }
}

object RedisClusterBenchmark {
  final val SequencedFutures = 200
}
