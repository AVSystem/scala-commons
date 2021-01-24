package com.avsystem.commons
package redis.commands

import java.nio.charset.StandardCharsets

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.exception.ErrorReplyException
import com.avsystem.commons.redis.protocol.ValidRedisMsg
import scala.annotation.nowarn
import com.google.common.hash.Hashing

trait KeyedScriptingApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/eval EVAL]] */
  def eval[T](script: RedisScript[T], keys: Seq[Key], args: Seq[Value]): Result[T] =
    execute(new Eval(script, keys, args))

  /** Executes [[http://redis.io/commands/eval EVAL]] */
  def eval[T](source: String, keys: Seq[Key], args: Seq[Value])(decoder: ReplyDecoder[T]): Result[T] =
    execute(new Eval(RedisScript(source)(decoder), keys, args))

  /** Executes [[http://redis.io/commands/evalsha EVALSHA]] */
  def evalsha[T](script: RedisScript[T], keys: Seq[Key], args: Seq[Value]): Result[T] =
    execute(new Evalsha(script.sha1, script.decoder, keys, args))

  /** Executes [[http://redis.io/commands/evalsha EVALSHA]] */
  def evalsha[T](sha1: Sha1, keys: Seq[Key], args: Seq[Value])(decoder: ReplyDecoder[T]): Result[T] =
    execute(new Evalsha(sha1, decoder, keys, args))

  private final class Eval[T](script: RedisScript[T], keys: Seq[Key], args: Seq[Value])
    extends AbstractRedisCommand[T](script.decoder) with NodeCommand {
    val encoded: Encoded = encoder("EVAL").add(script.source).add(keys.size).keys(keys).datas(args).result
  }

  private final class Evalsha[T](sha1: Sha1, decoder: PartialFunction[ValidRedisMsg, T], keys: Seq[Key], args: Seq[Value])
    extends AbstractRedisCommand[T](decoder) with NodeCommand {
    val encoded: Encoded = encoder("EVALSHA").add(sha1.raw).add(keys.size).keys(keys).datas(args).result
  }
}

trait RecoverableKeyedScriptingApi extends RecoverableApiSubset with KeyedScriptingApi {
  /**
    * Tries to execute [[http://redis.io/commands/evalsha EVALSHA]]
    * and falls back to [[http://redis.io/commands/eval EVAL]]
    * if script isn't loaded yet.
    */
  def evalshaOrEval[T](script: RedisScript[T], keys: Seq[Key], args: Seq[Value]): Result[T] =
    recoverWith(evalsha(script, keys, args)) {
      case e: ErrorReplyException if e.reply.errorCode == "NOSCRIPT" =>
        eval(script, keys, args)
    }
}

trait NodeScriptingApi extends KeyedScriptingApi {
  /** [[http://redis.io/commands/script-exists SCRIPT EXISTS]] */
  def scriptExists(hash: Sha1): Result[Boolean] =
    execute(new ScriptExists(hash.single).map(_.head))

  /** [[http://redis.io/commands/script-exists SCRIPT EXISTS]] */
  def scriptExists(hash: Sha1, hashes: Sha1*): Result[Seq[Boolean]] =
    execute(new ScriptExists(hash +:: hashes))

  /** Executes [[http://redis.io/commands/script-exists SCRIPT EXISTS]]
    * NOTE: `hashes` CAN be empty, Redis accepts it */
  def scriptExists(hashes: Iterable[Sha1]): Result[Seq[Boolean]] =
    execute(new ScriptExists(hashes))

  /** Executes [[http://redis.io/commands/script-flush SCRIPT FLUSH]] */
  def scriptFlush: Result[Unit] =
    execute(ScriptFlush)

  /** Executes [[http://redis.io/commands/script-kill SCRIPT KILL]] */
  def scriptKill: Result[Unit] =
    execute(ScriptKill)

  /** Executes [[http://redis.io/commands/script-load SCRIPT LOAD]] */
  def scriptLoad(script: RedisScript[Any]): Result[Sha1] =
    execute(new ScriptLoad(script))

  private final class ScriptExists(hashes: Iterable[Sha1])
    extends RedisSeqCommand[Boolean](integerAsBoolean) with NodeCommand {
    val encoded: Encoded = encoder("SCRIPT", "EXISTS").add(hashes).result
  }

  private object ScriptFlush extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("SCRIPT", "FLUSH").result
  }

  private object ScriptKill extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("SCRIPT", "KILL").result
  }

  private final class ScriptLoad(script: RedisScript[Any])
    extends AbstractRedisCommand[Sha1](bulkAsSha1) with NodeCommand {
    val encoded: Encoded = encoder("SCRIPT", "LOAD").add(script.source).result
  }
}

trait ConnectionScriptingApi extends NodeScriptingApi {
  /** Executes [[http://redis.io/commands/script-debug SCRIPT DEBUG]] */
  def scriptDebug(mode: DebugMode): Result[Unit] =
    execute(new ScriptDebug(mode))

  private final class ScriptDebug(mode: DebugMode) extends RedisUnitCommand with ConnectionCommand {
    val encoded: Encoded = encoder("SCRIPT", "DEBUG").add(mode).result
  }
}

trait RedisScript[+A] {
  def source: String
  def decoder: ReplyDecoder[A]
  lazy val sha1: Sha1 = Sha1.hashString(source)
}
object RedisScript {
  def apply[A](script: String)(replyDecoder: ReplyDecoder[A]): RedisScript[A] =
    new RedisScript[A] {
      def source: String = script
      def decoder: ReplyDecoder[A] = replyDecoder
    }
}
case class Sha1(raw: String) extends AnyVal {
  override def toString: String = raw
}
object Sha1 {
  @nowarn
  def hashString(input: CharSequence): Sha1 =
    Sha1(Hashing.sha1.hashString(input, StandardCharsets.UTF_8).toString)

  implicit val commandArg: CommandArg[Sha1] =
    CommandArg((enc, sha1) => enc.add(sha1.raw))
}

sealed abstract class DebugMode(val name: String) extends NamedEnum
object DebugMode extends NamedEnumCompanion[DebugMode] {
  case object Yes extends DebugMode("YES")
  case object Sync extends DebugMode("SYNC")
  case object No extends DebugMode("NO")

  val values: List[DebugMode] = caseObjects
}
