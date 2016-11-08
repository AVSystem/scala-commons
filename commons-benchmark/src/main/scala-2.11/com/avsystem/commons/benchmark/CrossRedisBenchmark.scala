package com.avsystem.commons
package benchmark

import _root_.redis._
import com.avsystem.commons.redis.RedisClientBenchmark
import com.avsystem.commons.redis.RedisClientBenchmark._
import org.openjdk.jmh.annotations.{Benchmark, OperationsPerInvocation}

import scala.concurrent.Future

trait CrossRedisBenchmark { this: RedisClientBenchmark =>
  lazy val rediscalaClient = RedisClient()
  lazy val rediscalaClientPool = RedisClientPool(Seq.fill(PoolSize)(RedisServer()))

  def rediscalaCommandFuture(client: RedisCommands, i: Int): Future[Any] =
    client.set(s"$KeyBase$i", "v")

  @Benchmark
  @OperationsPerInvocation(ConcurrentCommands)
  def rediscalaCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, rediscalaCommandFuture(rediscalaClient, _))

  @Benchmark
  @OperationsPerInvocation(ConcurrentCommands)
  def rediscalaPoolCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, rediscalaCommandFuture(rediscalaClientPool, _))
}
