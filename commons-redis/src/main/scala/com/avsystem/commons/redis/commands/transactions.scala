package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.protocol.{ArrayMsg, ErrorMsg, NullArrayMsg, RedisMsg}
import com.avsystem.commons.redis.{ApiSubset, OperationCommand, RedisUnitCommand, UnsafeCommand, WatchState}

trait TransactionApi extends ApiSubset {
  def watch(keys: Seq[Key]): Result[Unit] =
    execute(Watch(keys))
  def unwatch: Result[Unit] =
    execute(Unwatch)

  case class Watch(keys: Seq[Key]) extends RedisUnitCommand with OperationCommand {
    val encoded = encoder("WATCH").keys(keys).result
    override def updateWatchState(message: RedisMsg, state: WatchState) = message match {
      case RedisMsg.Ok => state.watching = true
      case _ =>
    }
  }

  case object Unwatch extends RedisUnitCommand with OperationCommand {
    val encoded = encoder("UNWATCH").result
    override def updateWatchState(message: RedisMsg, state: WatchState) = message match {
      case RedisMsg.Ok => state.watching = false
      case _ =>
    }
  }
}

case object Multi extends UnsafeCommand {
  val encoded = encoder("MULTI").result
}

case object Exec extends UnsafeCommand {
  val encoded = encoder("EXEC").result
  override def updateWatchState(message: RedisMsg, state: WatchState) = message match {
    case _: ArrayMsg[RedisMsg] | NullArrayMsg => state.watching = false
    case err: ErrorMsg if err.errorCode == "EXECABORT" => state.watching = false
    case _ =>
  }
}

case object Discard extends UnsafeCommand {
  val encoded = encoder("DISCARD").result
  override def updateWatchState(message: RedisMsg, state: WatchState) = message match {
    case RedisMsg.Ok => state.watching = false
    case _ =>
  }
}
