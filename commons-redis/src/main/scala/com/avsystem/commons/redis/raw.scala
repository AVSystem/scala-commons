package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.collection.CrossArraySeqFactory
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.exception.ForbiddenCommandException
import com.avsystem.commons.redis.protocol.{ArrayMsg, BulkStringMsg, RedisMsg, RedisReply}

/**
  * One or more raw Redis commands. More lightweight than regular Scala collection
  * (avoids wrapping in case of single element).
  */
trait RawCommands {
  def emitCommands(consumer: RawCommand => Unit): Unit
}

trait RawCommand extends RawCommandPack with RawCommands with ReplyPreprocessor {
  type Encoded = ArrayMsg[BulkStringMsg]
  def encoded: Encoded
  def updateWatchState(message: RedisMsg, state: WatchState): Unit = ()
  def level: Level
  override def maxBlockingMillis: Int = 0

  final def checkLevel(minAllowed: Level, clientType: String): Unit =
    if (!minAllowed.allows(level)) {
      throw new ForbiddenCommandException(this, clientType)
    }

  final def rawCommands(inTransaction: Boolean): RawCommand = this
  final def emitCommands(consumer: RawCommand => Unit): Unit = consumer(this)
  final def createPreprocessor(replyCount: Int): RawCommand = this
  final def preprocess(message: RedisMsg, state: WatchState): Opt[RedisMsg] = {
    updateWatchState(message, state)
    Opt(message)
  }

  override final def isAsking = false

  protected final def encoder(command: String): CommandEncoder =
    new CommandEncoder(CrossArraySeqFactory.newBuilder).add(command)

  protected final def encoder(command: String, subcommand: String): CommandEncoder =
    new CommandEncoder(CrossArraySeqFactory.newBuilder).add(command).add(subcommand)
}

trait UnsafeCommand extends RawCommand {
  final def level: Level = Level.Unsafe
}
trait ConnectionCommand extends RawCommand {
  final def level: Level = Level.Connection
}
trait OperationCommand extends RawCommand {
  final def level: Level = Level.Operation
}
trait NodeCommand extends RawCommand {
  final def level: Level = Level.Node
}

object RawCommand {
  case class Level(raw: Int) extends AnyVal {
    def allows(other: Level): Boolean = raw <= other.raw
  }
  object Level {
    final val Unsafe = Level(0)
    final val Connection = Level(1)
    final val Operation = Level(2)
    final val Node = Level(3)
  }
}

/**
  * One or more [[RawCommandPack]]s. Conceptually pretty much the same as `Iterable[RawCommandPack]]`
  * but more lightweight.
  */
trait RawCommandPacks {
  def emitCommandPacks(consumer: RawCommandPack => Unit): Unit
  def computeSize(limit: Int): Int

  /**
    * Maximum amount of time that these command packs may block on Redis side (like e.g. `BLPOP`).
    * This method is overridden for [[RawCommand]] to return 0 and should be further overridden by each
    * blocking command. `Int.MaxValue` should be returned for unlimited blocking.
    */
  def maxBlockingMillis: Int = {
    val counter = new RawCommandPacks.MaxBlockingMillisCounter
    emitCommandPacks(counter)
    counter.result
  }

  final def foreachKey(consumer: ByteString => Unit): Unit =
    emitCommandPacks(_.rawCommands(inTransaction = false)
      .emitCommands(_.encoded.elements.foreach(bs => if (bs.isCommandKey) consumer(bs.string))))

  final def encodedSize: Int = {
    var result = 0
    emitCommandPacks(_.rawCommands(inTransaction = false)
      .emitCommands(c => result += RedisMsg.encodedSize(c.encoded)))
    result
  }

  final def requireLevel(minAllowed: Level, clientType: String): this.type = {
    emitCommandPacks(_.checkLevel(minAllowed, clientType))
    this
  }
}
object RawCommandPacks {
  private final class MaxBlockingMillisCounter extends (RawCommandPack => Unit) {
    private[this] var acc = 0
    def result: Int = acc

    def apply(pack: RawCommandPack): Unit = {
      val toadd = pack.maxBlockingMillis
      acc += math.min(Int.MaxValue - acc, toadd)
    }
  }
}

/**
  * Represents a sequence of commands that is always executed atomically, using a single network call
  * on a single Redis connection. [[RawCommandPack]] is effectively either a single command or a transaction,
  * optionally preceded by the `ASKING` special command.
  */
trait RawCommandPack extends RawCommandPacks {
  def rawCommands(inTransaction: Boolean): RawCommands
  def createPreprocessor(replyCount: Int): ReplyPreprocessor
  def checkLevel(minAllowed: Level, clientType: String): Unit

  def isAsking: Boolean = false
  final def emitCommandPacks(consumer: RawCommandPack => Unit): Unit = consumer(this)
  final def computeSize(limit: Int): Int = limit min 1
}

trait WatchState {
  var watching: Boolean = false
}

/**
  * Something that translates incoming [[protocol.RedisMsg RedisMsg]]
  * messages and emits a single [[protocol.RedisReply RedisReply]].
  * For example, it may handle transactions by extracting actual responses for every command from
  * the `EXEC` response and returning them in a [[protocol.TransactionReply TransactionReply]]
  * (see [[Transaction]]).
  */
trait ReplyPreprocessor {
  def preprocess(message: RedisMsg, watchState: WatchState): Opt[RedisReply]
}
