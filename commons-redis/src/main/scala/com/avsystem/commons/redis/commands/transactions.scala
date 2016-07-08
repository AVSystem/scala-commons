package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis.RedisBatch.ConnectionState
import com.avsystem.commons.redis.protocol.RedisMsg
import com.avsystem.commons.redis.{OperationApiSubset, RedisRawCommand, RedisUnitCommand, Scope}

trait TransactionApi extends OperationApiSubset {
  def watch(keys: Seq[ByteString]) =
    execute(Watch(keys))
  def unwatch =
    execute(Unwatch)
}

case class Watch(keys: Seq[ByteString]) extends RedisUnitCommand[Scope.Operation] {
  def encode = encoder("WATCH").keys(keys).result

  override def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int, state: ConnectionState) = {
    super.decodeReplies(replies, start, end, state)
    state.watching = true
  }
}

case object Unwatch extends RedisUnitCommand[Scope.Operation] {
  def encode = encoder("UNWATCH").result

  override def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int, state: ConnectionState) = {
    super.decodeReplies(replies, start, end, state)
    state.watching = false
  }
}

case object Multi extends RedisUnitCommand[Scope.Empty] {
  def encode = encoder("MULTI").result
}

case object Exec extends RedisRawCommand[Scope.Empty] {
  def encode = encoder("EXEC").result
}

case object Discard extends RedisUnitCommand[Scope.Empty] {
  def encode = encoder("DISCARD").result
}
