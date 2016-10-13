package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.protocol.{ArrayMsg, ErrorMsg, NullArrayMsg, RedisMsg}
import com.avsystem.commons.redis.{ApiSubset, OperationCommand, RedisUnitCommand, UnsafeCommand, WatchState}

trait TransactionApi extends ApiSubset {
  def watch(key: Key, keys: Key*): Result[Unit] =
    execute(new Watch(key +:: keys))
  def watch(keys: Iterable[Key]): Result[Unit] =
    execute(new Watch(keys))
  def unwatch: Result[Unit] =
    execute(Unwatch)

  private final class Watch(keys: Iterable[Key]) extends RedisUnitCommand with OperationCommand {
    requireNonEmpty(keys, "keys")
    val encoded = encoder("WATCH").keys(keys).result
    override def updateWatchState(message: RedisMsg, state: WatchState) = message match {
      case RedisMsg.Ok => state.watching = true
      case _ =>
    }
  }

  private object Unwatch extends RedisUnitCommand with OperationCommand {
    val encoded = encoder("UNWATCH").result
    override def updateWatchState(message: RedisMsg, state: WatchState) = message match {
      case RedisMsg.Ok => state.watching = false
      case _ =>
    }
  }
}

private[redis] object Multi extends UnsafeCommand {
  val encoded = encoder("MULTI").result
}

private[redis] object Exec extends UnsafeCommand {
  val encoded = encoder("EXEC").result
  override def updateWatchState(message: RedisMsg, state: WatchState) = message match {
    case _: ArrayMsg[RedisMsg] | NullArrayMsg => state.watching = false
    case err: ErrorMsg if err.errorCode == "EXECABORT" => state.watching = false
    case _ =>
  }
}

private[redis] object Discard extends UnsafeCommand {
  val encoded = encoder("DISCARD").result
  override def updateWatchState(message: RedisMsg, state: WatchState) = message match {
    case RedisMsg.Ok => state.watching = false
    case _ =>
  }
}
