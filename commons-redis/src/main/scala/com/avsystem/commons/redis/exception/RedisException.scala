package com.avsystem.commons
package redis.exception

import com.avsystem.commons.redis.protocol.ErrorMsg
import com.avsystem.commons.redis.{NodeAddress, RawCommand, Redirection}

class RedisException(msg: String = null, cause: Throwable = null)
  extends RuntimeException(msg, cause)

/**
  * Throw when response sent back by Redis server is corrupt according to Redis protocol.
  */
class InvalidDataException(msg: String)
  extends RedisException(msg)

/**
  * Thrown when transactions fails due to one of watched keys having been modified by another client,
  * i.e. when `EXEC` command in a `WATCH`-`MULTI`-`EXEC` transaction returns null bulk response.
  */
class OptimisticLockException
  extends RedisException("One of the watched keys has been modified by another client")

/**
  * Thrown when Redis server returns a reply unexpected by a decoder of a particular command.
  */
class UnexpectedReplyException(msg: String = null)
  extends RedisException(msg)

/**
  * Thrown when Redis server replies with an error.
  */
class ErrorReplyException(val reply: ErrorMsg)
  extends RedisException(reply.errorString.utf8String)

class RedisIOException(msg: String = null, cause: Throwable = null)
  extends RedisException(msg, cause)

class ConnectionFailedException(val address: NodeAddress)
  extends RedisIOException(s"Failed to connect to Redis instance at $address")

/**
  * Thrown when an I/O error occ
  */
class WriteFailedException(val address: NodeAddress)
  extends RedisIOException(s"Failed to send data through Redis connection to $address")

/**
  * Command or operation is failed with this exception when it has been already sent through network but the
  * connection was closed before receiving a response. Even though connections are automatically restarted,
  * such command cannot be resent because we don't know whether it was actually executed on the Redis instance or not.
  * In a Redis Cluster deployment, this is likely to happen when a node fails.
  */
class ConnectionClosedException(val address: NodeAddress, val cause: Opt[String])
  extends RedisIOException(s"Redis connection to $address was closed${cause.fold("")(c => s": $c")}")

class ConnectionBusyException(val address: NodeAddress)
  extends RedisIOException(s"Redis connection to $address is currently busy writing other request")

class ClientStoppedException(val address: Opt[NodeAddress])
  extends RedisException(s"Redis client for ${address.fold("cluster")(_.toString)} was stopped")

/**
  * Thrown when some command was queued for execution on a [[com.avsystem.commons.redis.RedisNodeClient RedisNodeClient]]
  * connected to one of the master nodes in Redis Cluster and [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]]
  * detected that this node is no longer a master before the command could be sent to it.
  */
class NodeRemovedException(val address: NodeAddress)
  extends RedisException(s"Node $address is no longer a master in Redis Cluster")

class ConnectionInitializationFailure(cause: Throwable)
  extends RedisException(s"Failed to initialize Redis connection", cause)

class NodeInitializationFailure(cause: Throwable)
  extends RedisException(s"Failed to initialize node client", cause)

/**
  * Thrown when trying to execute command unsupported by particular client type. For example, it's impossible
  * to execute connection state changing commands like `CLIENT SETNAME` using a
  * [[com.avsystem.commons.redis.RedisNodeClient RedisNodeClient]].
  */
class ForbiddenCommandException(cmd: RawCommand, client: String)
  extends RedisException(s"This command cannot be executed on $client: $cmd")

/**
  * Thrown when [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]] is unable to fetch initial
  * cluster state from any of the seed nodes. This happens e.g. when none of the seed nodes can be contacted or when
  * they aren't Redis Cluster members.
  *
  * @param seedNodes seed node addresses passed to [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]]
  */
class ClusterInitializationException(val seedNodes: Seq[NodeAddress])
  extends RedisException(s"Failed to read cluster state from any of the seed nodes ${seedNodes.mkString(",")}")

/**
  * Thrown when some multi-keyed command or `MULTI`-`EXEC` block executed by
  * [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]] contains keys that hash to different slots.
  */
class CrossSlotException
  extends RedisException("Keys don't hash to the same slot")

/**
  * Throw when (according to current cluster state) some slot is not served by any master. This most likely
  * indicates problem with setup of the Redis Cluster deployment itself.
  */
class UnmappedSlotException(val slot: Int)
  extends RedisException(s"Slot $slot is not served by any node")

/**
  * Thrown when trying to execute command or `MULTI`-`EXEC` block that does not contain any keys using
  * [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]].
  */
class NoKeysException
  extends RedisException(s"Cannot execute commands with no keys using cluster client")

/**
  * Thrown when too many consecutive cluster redirections occurred during execution of some command by
  * [[com.avsystem.commons.redis.RedisClusterClient RedisClusterClient]]. This might indicate a problem
  * with configuration of Redis Cluster deployment itself.
  *
  * Maximum number of consecutive redirections is configured by
  * [[com.avsystem.commons.redis.config.ClusterConfig ClusterConfig]]
  */
class TooManyRedirectionsException(val lastRedirection: Redirection)
  extends RedisException(s"Too many Redis cluster redirections, last one to: " +
    s"${lastRedirection.address} (slot ${lastRedirection.slot}${if (lastRedirection.ask) ", ASK" else ""})")
