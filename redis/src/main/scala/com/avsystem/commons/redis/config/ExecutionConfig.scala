package com.avsystem.commons
package redis.config

import org.apache.pekko.util.Timeout
import com.avsystem.commons.concurrent.RunNowEC

import scala.concurrent.duration._

/**
  * Additional options for executing a [[com.avsystem.commons.redis.RedisBatch RedisBatch]] on a
  * [[com.avsystem.commons.redis.RedisExecutor RedisExecutor]]
  *
  * @param responseTimeout Redis server response timeout. If executing a batch involves retries (e.g. because of
  *                        cluster redirections) then timeout is applied independently on every retry.
  * @param decodeOn        execution context on which Redis response to a batch will be decoded. Normally this is happening
  *                        on one of the connection actor threads. This is ok for simple Redis commands but may introduce
  *                        performance bottleneck for large batches with more heavy decoding. In such case it may be
  *                        beneficial to delegate that work to some external executor.
  */
case class ExecutionConfig(
  responseTimeout: Timeout = 10.seconds,
  decodeOn: ExecutionContext = RunNowEC
)
object ExecutionConfig {
  final val Default = ExecutionConfig()
}
