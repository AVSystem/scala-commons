package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.protocol.{ArrayMsg, BulkStringMsg, RedisMsg, RedisReply}

import scala.collection.mutable.ArrayBuffer

/**
  * One or more raw Redis commands. More lightweight than regular Scala collection
  * (avoids wrapping in case of single element).
  */
trait RawCommands {
  def emitCommands(consumer: ArrayMsg[BulkStringMsg] => Unit): Unit
}

trait RawCommand extends RawCommandPack with RawCommands with ReplyPreprocessor {
  val encoded: ArrayMsg[BulkStringMsg]
  def updateState(message: RedisMsg, state: ConnectionState): Unit = ()

  def rawCommands(inTransaction: Boolean) = this
  def emitCommands(consumer: ArrayMsg[BulkStringMsg] => Unit) = consumer(encoded)
  def createPreprocessor(replyCount: Int) = this
  def preprocess(message: RedisMsg, state: ConnectionState) = {
    updateState(message, state)
    Opt(message)
  }

  protected def encoder(commandName: String*): CommandEncoder = {
    val res = new CommandEncoder(new ArrayBuffer)
    res.add(commandName)
    res
  }
}

/**
  * One or more [[RawCommandPack]]s. More lightweight than regular Scala collection
  * (avoids wrapping in case of single element).
  */
trait RawCommandPacks {
  def emitCommandPacks(consumer: RawCommandPack => Unit): Unit
}

/**
  * Represents a sequence of commands that is always executed atomically, using a single network call
  * on a single Redis connection.
  */
trait RawCommandPack extends RawCommandPacks {
  def rawCommands(inTransaction: Boolean): RawCommands
  def createPreprocessor(replyCount: Int): ReplyPreprocessor

  def emitCommandPacks(consumer: RawCommandPack => Unit) = consumer(this)
}

final class ConnectionState {
  var watching: Boolean = false
}

/**
  * Something that translates incoming [[RedisMsg]] messages and emits a single [[RedisReply]].
  * For example, it may handle transactions by extracting actual responses for every command from
  * the `EXEC` response and returning them in a [[com.avsystem.commons.redis.protocol.TransactionReply]] (see [[Transaction]]).
  */
trait ReplyPreprocessor {
  def preprocess(message: RedisMsg, connectionState: ConnectionState): Opt[RedisReply]
}
