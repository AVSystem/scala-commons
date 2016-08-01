package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.commands.{Exec, Multi}
import com.avsystem.commons.redis.exception.{OptimisticLockException, RedisException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ArrayBuffer

final class Transaction[+A, -S](batch: RedisBatch[A, S]) extends AtomicBatch[A, S] {

  def rawCommands(inTransaction: Boolean) = new RawCommands {
    def emitCommands(consumer: ArrayMsg[BulkStringMsg] => Unit) = {
      if (!inTransaction) {
        consumer(Multi.encoded)
      }
      batch.rawCommandPacks.emitCommandPacks(_.rawCommands(inTransaction = true).emitCommands(consumer))
      if (!inTransaction) {
        consumer(Exec.encoded)
      }
    }
  }

  def createPreprocessor(replyCount: Int) = new ReplyPreprocessor {
    private var singleError: Opt[FailureReply] = Opt.Empty
    private var errors: Opt[ArrayBuffer[ErrorMsg]] = Opt.Empty
    private var normalResult: Opt[ArrayMsg[RedisMsg]] = Opt.Empty
    private var ctr = 0

    private def setSingleError(exception: => RedisException): Unit =
      if (singleError.isEmpty) {
        singleError = FailureReply(exception).opt
      }

    private def errorsBuffer: ArrayBuffer[ErrorMsg] =
      errors.getOrElse {
        errors = ArrayBuffer.fill[ErrorMsg](replyCount - 2)(null).opt
        errorsBuffer
      }

    private def setDefaultError(fillWith: ErrorMsg): Unit = {
      val buf = errorsBuffer
      var i = 0
      while (i < buf.length) {
        if (buf(i) == null) {
          buf(i) = fillWith
        }
        i += 1
      }
    }

    def preprocess(message: RedisMsg, state: ConnectionState) = {
      val LastIndex = replyCount - 1
      val c = ctr
      ctr += 1
      c match {
        case 0 =>
          message match {
            case RedisMsg.Ok =>
            case _ => setSingleError(new UnexpectedReplyException(s"Unexpected reply for MULTI: $message"))
          }
          Opt.Empty
        case LastIndex =>
          Exec.updateState(message, state)
          message match {
            case arr: ArrayMsg[RedisMsg] => normalResult = arr.opt
            case NullArrayMsg => setSingleError(new OptimisticLockException)
            case errorMsg: ErrorMsg => setDefaultError(errorMsg)
            case _ => setSingleError(new UnexpectedReplyException(s"Unexpected reply for EXEC: $message"))
          }
          singleError orElse errors.map(ArrayMsg(_)) orElse normalResult
        case i =>
          message match {
            case RedisMsg.Queued =>
            case errorMsg: ErrorMsg =>
              errorsBuffer(i - 1) = errorMsg
            case _ =>
              setSingleError(new UnexpectedReplyException(s"Unexpected reply: expected QUEUED, got $message"))
          }
          Opt.Empty
      }
    }
  }

  def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) =
    if (inTransaction) batch.decodeReplies(replies, index, inTransaction)
    else replies(index.inc()) match {
      case ArrayMsg(elements) =>
        batch.decodeReplies(elements, new Index, inTransaction = true)
      case fr: FailureReply =>
        batch.decodeReplies(_ => fr, new Index, inTransaction = true)
      case msg =>
        val failure = FailureReply(new UnexpectedReplyException(s"Unexpected reply for transaction: $msg"))
        batch.decodeReplies(_ => failure, new Index, inTransaction = true)
    }

  override def transaction = this
}
