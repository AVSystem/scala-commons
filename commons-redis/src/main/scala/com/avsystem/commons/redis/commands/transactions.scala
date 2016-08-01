package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis.protocol.{ArrayMsg, ErrorMsg, NullArrayMsg, RedisMsg}
import com.avsystem.commons.redis.{ConnectionState, OperationApiSubset, RawCommand, RedisUnitCommand, Scope}

trait TransactionApi extends OperationApiSubset {
  def watch(keys: Seq[ByteString]) =
    execute(Watch(keys))
  def unwatch =
    execute(Unwatch)
}

case class Watch(keys: Seq[ByteString]) extends RedisUnitCommand[Scope.Operation] {
  val encoded = encoder("WATCH").keys(keys).result
  override def updateState(message: RedisMsg, state: ConnectionState) = message match {
    case RedisMsg.Ok => state.watching = true
    case _ =>
  }
}

case object Unwatch extends RedisUnitCommand[Scope.Operation] {
  val encoded = encoder("UNWATCH").result
  override def updateState(message: RedisMsg, state: ConnectionState) = message match {
    case RedisMsg.Ok => state.watching = false
    case _ =>
  }
}

case object Multi extends RawCommand {
  val encoded = encoder("MULTI").result
}

case object Exec extends RawCommand {
  val encoded = encoder("EXEC").result
  override def updateState(message: RedisMsg, state: ConnectionState) = message match {
    case _: ArrayMsg[RedisMsg] | NullArrayMsg => state.watching = false
    case err: ErrorMsg if err.errorCode == "EXECABORT" => state.watching = false
    case _ =>
  }
}

case object Discard extends RawCommand {
  val encoded = encoder("DISCARD").result
  override def updateState(message: RedisMsg, state: ConnectionState) = message match {
    case RedisMsg.Ok => state.watching = false
    case _ =>
  }
}
