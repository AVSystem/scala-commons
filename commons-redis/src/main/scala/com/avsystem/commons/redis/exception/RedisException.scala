package com.avsystem.commons
package redis.exception

import com.avsystem.commons.redis.NodeAddress
import com.avsystem.commons.redis.protocol.ErrorMsg

/**
  * Author: ghik
  * Created: 08/04/16.
  */
class RedisException(msg: String = null, cause: Throwable = null)
  extends RuntimeException(msg, cause)

class InvalidDataException(msg: String)
  extends RedisException(msg)

class OptimisticLockException
  extends RedisException("One of the watched keys has been modified by another client")

class UnexpectedReplyException(msg: String = null)
  extends RedisException(msg)

class ErrorReplyException(val reply: ErrorMsg)
  extends RedisException(reply.errorString.utf8String)

class RedisIOException(msg: String = null, cause: Throwable = null)
  extends RedisException(msg, cause)

class ConnectionFailedException(val address: NodeAddress)
  extends RedisIOException(s"Failed to connect to Redis instance at $address")

class WriteFailedException(val address: NodeAddress)
  extends RedisIOException(s"Failed to send data through Redis connection to $address")

class ConnectionClosedException(val address: NodeAddress)
  extends RedisIOException(s"Redis connection to $address was closed")

class ClientStoppedException(val address: NodeAddress)
  extends RedisException(s"Redis client for $address was stopped")

class ConnectionReservedException
  extends RedisException("This connection is already reserved by somebody else")

class ConnectionInitializationFailure(cause: Throwable)
  extends RedisException(s"Failed to initialize Redis connection", cause)

class ConnectionStateResetFailure(cause: Throwable)
  extends RedisException("Failure while resetting Redis connection state", cause)

class CrossSlotException
  extends RedisException("Keys don't hash to the same slot")

class UnmappedSlotException(val slot: Int)
  extends RedisException(s"Slot $slot is not served by any node")

class NoKeysException
  extends RedisException(s"Cannot execute batch with no keys on the cluster client")

class TooManyRedirectionsException(val address: NodeAddress, val slot: Int, val ask: Boolean)
  extends RedisException(s"Too many Redis cluster redirections, last one to: $address (slot $slot${if (ask) ", ASK" else ""})")
