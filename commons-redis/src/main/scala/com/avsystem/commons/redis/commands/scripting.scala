package com.avsystem.commons
package redis.commands

import java.nio.charset.StandardCharsets

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.exception.ErrorReplyException
import com.avsystem.commons.redis.protocol.ValidRedisMsg
import com.avsystem.commons.redis._
import com.google.common.hash.Hashing

/**
  * Author: ghik
  * Created: 04/10/16.
  */
trait KeyedScriptingApi extends ApiSubset {
  def eval[T](script: RedisScript[T], keys: Seq[Key], args: Seq[Value]): Result[T] =
    execute(new Eval(script, keys, args))
  def eval[T](source: String, keys: Seq[Key], args: Seq[Value])(decoder: ReplyDecoder[T]): Result[T] =
    execute(new Eval(RedisScript(source)(decoder), keys, args))
  def evalsha[T](script: RedisScript[T], keys: Seq[Key], args: Seq[Value]): Result[T] =
    execute(new Evalsha(script.sha1, script.decoder, keys, args))
  def evalsha[T](sha1: Sha1, keys: Seq[Key], args: Seq[Value])(decoder: ReplyDecoder[T]): Result[T] =
    execute(new Evalsha(sha1, decoder, keys, args))

  private final class Eval[T](script: RedisScript[T], keys: Seq[Key], args: Seq[Value])
    extends AbstractRedisCommand[T](script.decoder) with NodeCommand {
    val encoded = encoder("EVAL").add(script.source).add(keys.size).keys(keys).datas(args).result
  }

  private final class Evalsha[T](sha1: Sha1, decoder: PartialFunction[ValidRedisMsg, T], keys: Seq[Key], args: Seq[Value])
    extends AbstractRedisCommand[T](decoder) with NodeCommand {
    val encoded = encoder("EVALSHA").add(sha1.raw).add(keys.size).keys(keys).datas(args).result
  }
}

trait RecoverableKeyedScriptingApi extends RecoverableApiSubset with KeyedScriptingApi {
  def evalshaOrEval[T](script: RedisScript[T], keys: Seq[Key], args: Seq[Value]): Result[T] =
    recoverWith(evalsha(script, keys, args)) {
      case e: ErrorReplyException if e.reply.errorCode == "NOSCRIPT" =>
        eval(script, keys, args)
    }
}

trait NodeScriptingApi extends KeyedScriptingApi {
  def scriptExists(hashes: Sha1*): Result[Seq[Boolean]] =
    execute(new ScriptExists(hashes))
  def scriptFlush: Result[Unit] =
    execute(ScriptFlush)
  def scriptKill: Result[Unit] =
    execute(ScriptKill)
  def scriptLoad(script: RedisScript[Any]): Result[Sha1] =
    execute(new ScriptLoad(script))

  private final class ScriptExists(hashes: Seq[Sha1])
    extends RedisSeqCommand[Boolean](integerBoolean) with NodeCommand {
    val encoded = encoder("SCRIPT", "EXISTS").add(hashes).result
  }

  private object ScriptFlush extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("SCRIPT", "FLUSH").result
  }

  private object ScriptKill extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("SCRIPT", "KILL").result
  }

  private final class ScriptLoad(script: RedisScript[Any])
    extends AbstractRedisCommand[Sha1](bulkSha1) with NodeCommand {
    val encoded = encoder("SCRIPT", "LOAD").add(script.source).result
  }
}

trait ConnectionScriptingApi extends NodeScriptingApi {
  def scriptDebug(mode: DebugMode): Result[Unit] =
    execute(new ScriptDebug(mode))

  private final class ScriptDebug(mode: DebugMode) extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("SCRIPT", "DEBUG").add(mode).result
  }
}

trait RedisScript[+A] {
  def source: String
  def decoder: ReplyDecoder[A]
  lazy val sha1 = Sha1(Hashing.sha1.hashString(source, StandardCharsets.UTF_8).toString)
}
object RedisScript {
  def apply[A](script: String)(replyDecoder: ReplyDecoder[A]): RedisScript[A] =
    new RedisScript[A] {
      def source = script
      def decoder = replyDecoder
    }
}
case class Sha1(raw: String) extends AnyVal {
  override def toString = raw
}
object Sha1 {
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
