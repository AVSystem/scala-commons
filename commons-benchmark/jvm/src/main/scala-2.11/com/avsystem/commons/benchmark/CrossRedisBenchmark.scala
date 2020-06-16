package com.avsystem.commons
package benchmark

import _root_.redis._
import com.avsystem.commons.redis.{AbstractRedisClientBenchmark, RedisClientBenchmark}
import com.avsystem.commons.redis.RedisClientBenchmark._
import org.openjdk.jmh.annotations.Benchmark

trait CrossRedisBenchmark { this: AbstractRedisClientBenchmark =>
  lazy val rediscalaClient = RedisClient()
  lazy val rediscalaClientPool = RedisClientPool(Seq.fill(nodeConfig.poolSize)(RedisServer()))

  lazy val scredisClient = scredis.Redis.withActorSystem()

  def rediscalaCommandFuture(client: RedisCommands, i: Int): Future[Any] =
    client.set(s"$KeyBase$i", Value)

  def scredisCommandFuture(client: scredis.Redis, i: Int): Future[Any] =
    client.set(s"$KeyBase$i", Value)

  def scredisTransactionFuture(client: scredis.Redis, i: Int): Future[Any] = {
    client.withTransaction { b =>
      for (j <- 0 until (BatchSize - 1)) {
        b.set(s"$KeyBase$i$j", Value)
      }
      b.set(s"$KeyBase$i${BatchSize - 1}", Value)
    }
  }

  @Benchmark
  def scredisCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, scredisCommandFuture(scredisClient, _))

  @Benchmark
  def scredisTransactionBenchmark() =
    redisClientBenchmark(ConcurrentBatches, scredisTransactionFuture(scredisClient, _))

  @Benchmark
  def rediscalaCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, rediscalaCommandFuture(rediscalaClient, _))

  @Benchmark
  def rediscalaPoolCommandBenchmark() =
    redisClientBenchmark(ConcurrentCommands, rediscalaCommandFuture(rediscalaClientPool, _))
}
