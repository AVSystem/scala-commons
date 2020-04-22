package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._
import com.avsystem.commons.redis.protocol.{ArrayMsg, ErrorMsg, NullArrayMsg, RedisMsg}

trait TransactionApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/watch WATCH]] */
  def watch(key: Key, keys: Key*): Result[Unit] =
    execute(new Watch(key +:: keys))
  /** Executes [[http://redis.io/commands/watch WATCH]]
    * or does nothing when `keys` is empty, without sending the command to Redis */
  def watch(keys: Iterable[Key]): Result[Unit] =
    execute(new Watch(keys))
  /** Executes [[http://redis.io/commands/unwatch UNWATCH]] */
  def unwatch: Result[Unit] =
    execute(Unwatch)

  private final class Watch(keys: Iterable[Key]) extends RedisUnitCommand with OperationCommand {
    val encoded: Encoded = encoder("WATCH").keys(keys).result
    override def updateWatchState(message: RedisMsg, state: WatchState): Unit = message match {
      case RedisMsg.Ok => state.watching = true
      case _ =>
    }
    override def immediateResult: Opt[Unit] = whenEmpty(keys, ())
  }

  private object Unwatch extends RedisUnitCommand with OperationCommand {
    val encoded: Encoded = encoder("UNWATCH").result
    override def updateWatchState(message: RedisMsg, state: WatchState): Unit = message match {
      case RedisMsg.Ok => state.watching = false
      case _ =>
    }
  }
}

private[redis] object Multi extends UnsafeCommand {
  val encoded: Encoded = encoder("MULTI").result
}

private[redis] object Exec extends UnsafeCommand {
  val encoded: Encoded = encoder("EXEC").result
  override def updateWatchState(message: RedisMsg, state: WatchState): Unit = message match {
    case _: ArrayMsg[RedisMsg] | NullArrayMsg => state.watching = false
    case err: ErrorMsg if err.errorCode == "EXECABORT" => state.watching = false
    case _ =>
  }
}

private[redis] object Discard extends UnsafeCommand {
  val encoded: Encoded = encoder("DISCARD").result
  override def updateWatchState(message: RedisMsg, state: WatchState): Unit = message match {
    case RedisMsg.Ok => state.watching = false
    case _ =>
  }
}
