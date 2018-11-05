package com.avsystem.commons
package redis.config

import java.net.InetSocketAddress

import akka.io.Inet
import akka.util.Timeout
import com.avsystem.commons.redis.actor.RedisConnectionActor.{DebugListener, DevNullListener}
import com.avsystem.commons.redis.config.RetryStrategy._
import com.avsystem.commons.redis.{NodeAddress, RedisBatch, RedisOp}

import scala.concurrent.duration._

/**
  * Configuration of a [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]]
  *
  * @param nodeConfigs                 function that returns [[NodeConfig]] given the address of the node.
  * @param monitoringConnectionConfigs function that returns [[ConnectionConfig]] for a monitoring connection
  *                                    used to monitor node with given address. The cluster client keeps single
  *                                    monitoring connection for every cluster master. Monitoring connections are used
  *                                    to refresh Redis Cluster state (current masters and slot mapping).
  * @param autoRefreshInterval         interval between routine cluster state refresh operations
  * @param minRefreshInterval          minimal interval between consecutive cluster state refresh operations.
  *                                    Normally, cluster state is not refreshed more frequently than specified by
  *                                    `autoRefreshInterval` but additional refresh operations may be forced when
  *                                    cluster redirections are observed. `minRefreshInterval` prevents too many
  *                                    refresh operations from being executed in such situations.
  * @param nodesToQueryForState        function that determines how many randomly selected masters should be queried
  *                                    for cluster state during routine state refresh operation. The function takes
  *                                    current number of known masters as its argument.
  * @param redirectionStrategy         [[RetryStrategy]] that controls Redis Cluster redirection handling
  *                                    (`MOVED` and `ASK` responses).
  * @param tryagainStrategy            [[RetryStrategy]] that controls retrying commands which failed with
  *                                    `TRYAGAIN` error which may be returned for multikey commands during
  *                                    cluster slot migration.
  * @param nodeClientCloseDelay        Delay after which [[com.avsystem.commons.redis.RedisNodeClient RedisNodeClient]]
  *                                    is closed when it's master leaves cluster state (goes down or becomes a slave).
  *                                    Note that the node client is NOT operational during that delay. Trying to
  *                                    execute commands on it will result in
  *                                    [[com.avsystem.commons.redis.exception.NodeRemovedException NodeRemovedException]]
  * @param fallbackToSingleNode        if [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]] has
  *                                    exactly one seed address configured and it points to a non-clustered Redis node
  *                                    then cluster client will not fail initialization but internally create a
  *                                    [[com.avsystem.commons.redis.RedisNodeClient RedisNodeClient]] for that node
  *                                    and forward all operations to it.
  */
case class ClusterConfig(
  nodeConfigs: NodeAddress => NodeConfig = _ => NodeConfig(),
  monitoringConnectionConfigs: NodeAddress => ConnectionConfig = _ => ConnectionConfig(),
  autoRefreshInterval: FiniteDuration = 5.seconds,
  minRefreshInterval: FiniteDuration = 1.seconds,
  nodesToQueryForState: Int => Int = _ min 5,
  redirectionStrategy: RetryStrategy = RetryStrategy.times(3),
  tryagainStrategy: RetryStrategy = exponentially(10.millis).maxDelay(5.seconds).maxTotal(1.minute),
  nodeClientCloseDelay: FiniteDuration = 1.seconds,
  fallbackToSingleNode: Boolean = false
)

/**
  * Configuration of a [[com.avsystem.commons.redis.RedisNodeClient RedisNodeClient]], used either as a standalone
  * client or internally by [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]].
  *
  * @param poolSize
  * Number of connections used by node client. Commands are distributed between connections using
  * a round-robin scenario. Number of connections in the pool is constant and cannot be changed.
  * Due to single-threaded nature of Redis, the number of concurrent connections should be kept
  * low for best performance. The only situation where the number of connections should be increased
  * is when using `WATCH`-`MULTI`-`EXEC` transactions with optimistic locking.
  * @param maxBlockingPoolSize
  * Maximum number of connections used by node client in order to handle blocking Redis
  * commands, e.g. `BLPOP`. Blocking commands may not be pipelined with other, independent
  * commands because these other commands may be delayed by the blocking command. Therefore
  * they require their own, dynamically resizable connection pool. Maximum size of that pool
  * is the limit of possible concurrent blocking commands that can be executed at the same time.
  * @param maxBlockingIdleTime
  * Maximum amount of time a blocking connection may be idle before being closed and removed from
  * the pool.
  * @param blockingCleanupInterval
  * Time interval between periodic blocking connection cleanup events,
  * with respect to [[maxBlockingIdleTime]].
  * @param initOp
  * A [[com.avsystem.commons.redis.RedisOp RedisOp]] executed by this client upon initialization.
  * This may be useful for things like script loading, especially when using cluster client which
  * may create and close node clients dynamically as reactions on cluster state changes.
  * @param initTimeout
  * Timeout used by initialization operation (`initOp`)
  * @param connectionConfigs
  * A function that returns [[ConnectionConfig]] for a connection given its id. Connection ID
  * is its index in the connection pool, i.e. an int ranging from 0 to `poolSize`-1.
  * @param blockingConnectionConfigs
  * Same as [[connectionConfigs]] but for connections used for handling blocking commands.
  */
case class NodeConfig(
  poolSize: Int = 1,
  maxBlockingPoolSize: Int = 4096,
  maxBlockingIdleTime: Duration = 1.minute,
  blockingCleanupInterval: FiniteDuration = 1.second,
  initOp: RedisOp[Any] = RedisOp.unit,
  initTimeout: Timeout = Timeout(10.seconds),
  connectionConfigs: Int => ConnectionConfig = _ => ConnectionConfig(),
  blockingConnectionConfigs: Int => ConnectionConfig = _ => ConnectionConfig()
) {
  require(poolSize > 0, "Pool size must be positive")
}

/**
  * Configuration options for a single Redis connection.
  *
  * `initCommands` usage example:
  * {{{
  *   implicit val actorSystem = ActorSystem()
  *   import RedisApi.Batches.StringTyped._
  *   val nodeClient = new RedisNodeClient(
  *     config = NodeConfig(
  *       poolSize = 8,
  *       connectionConfigs = connectionId => ConnectionConfig(
  *         initCommands = auth("mypassword") *> clientSetname(s"conn_$$connectionId") *> select(1)
  *       )
  *     )
  *   )
  * }}}
  *
  * @param initCommands         commands always sent upon establishing a Redis connection (and every time it's reconnected).
  *                             The most common reason to configure `initCommands` is to specify authentication password used by every
  *                             connection (`AUTH` command), but it's also useful for commands like `CLIENT SETNAME`, `SELECT`, etc.
  *                             Note that these are all commands that can't be executed directly by
  *                             [[com.avsystem.commons.redis.RedisNodeClient RedisNodeClient]] or
  *                             [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]].
  * @param actorName            name of the actor representing the connection
  * @param localAddress         local bind address for the connection
  * @param socketOptions        socket options for the connection
  * @param connectTimeout       timeout for establishing connection
  * @param maxWriteSizeHint     hint for maximum number of bytes sent in a single network write message (the actual number
  *                             of bytes sent may be slightly larger)
  * @param reconnectionStrategy a [[RetryStrategy]] used to determine what delay should be used when reconnecting
  *                             a failed connection. NOTE: `reconnectionStrategy` is ignored by `RedisConnectionClient`
  * @param debugListener        listener for traffic going through this connection. Only for debugging and testing
  *                             purposes
  */
case class ConnectionConfig(
  initCommands: RedisBatch[Any] = RedisBatch.unit,
  actorName: OptArg[String] = OptArg.Empty,
  localAddress: OptArg[InetSocketAddress] = OptArg.Empty,
  socketOptions: List[Inet.SocketOption] = Nil,
  connectTimeout: OptArg[FiniteDuration] = OptArg.Empty,
  maxWriteSizeHint: OptArg[Int] = 50000,
  reconnectionStrategy: RetryStrategy = immediately.andThen(exponentially(1.seconds)).maxDelay(8.seconds),
  debugListener: DebugListener = DevNullListener
)
