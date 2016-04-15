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

class ErrorReplyException(reply: ErrorMsg)
  extends RedisException(reply.errorString.utf8String)

class RedisIOException(msg: String = null, cause: Throwable = null)
  extends RedisException(msg, cause)

class ConnectionFailedException(address: NodeAddress)
  extends RedisIOException(s"Failed to connect to Redis instance at $address")

class WriteFailedException(address: NodeAddress)
  extends RedisIOException(s"Failed to send data through Redis connection to $address")

class ConnectionClosedException(address: NodeAddress)
  extends RedisIOException(s"Redis connection to $address was closed")

class ClientStoppedException(address: NodeAddress)
  extends RedisException(s"Redis client for $address was stopped")
