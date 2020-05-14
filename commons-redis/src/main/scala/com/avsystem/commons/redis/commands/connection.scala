package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait NodeConnectionApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/echo ECHO]] */
  def echo(message: ByteString): Result[ByteString] =
    execute(new Echo(message))

  /** Executes [[http://redis.io/commands/ping PING]] */
  def ping: Result[ByteString] =
    execute(Ping)

  /** Executes [[http://redis.io/commands/swapdb SWAPDB]] */
  def swapdb(first: Int, second: Int): Result[Unit] =
    execute(new Swapdb(first, second))

  private final class Echo(message: ByteString) extends RedisBinaryCommand with NodeCommand {
    val encoded: Encoded = encoder("ECHO").data(message).result
  }

  private object Ping extends AbstractRedisCommand[ByteString](simpleBinary) with NodeCommand {
    val encoded: Encoded = encoder("PING").result
  }

  private final class Swapdb(first: Int, second: Int) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("SWAPDB").add(first).add(second).result
  }
}

trait ConnectionConnectionApi extends NodeConnectionApi {
  /** Executes [[http://redis.io/commands/auth AUTH]] */
  def auth(password: String): Result[Unit] =
    execute(new Auth(Opt.Empty, password))

  /** Executes [[http://redis.io/commands/auth AUTH]] */
  def auth(username: String, password: String): Result[Unit] =
    execute(new Auth(Opt(username), password))

  /** Executes [[http://redis.io/commands/quit QUIT]] */
  def quit: Result[Unit] =
    execute(Quit)

  /** Executes [[http://redis.io/commands/select SELECT]] */
  def select(index: Int): Result[Unit] =
    execute(new Select(index))

  private final class Auth(username: Opt[String], password: String) extends RedisUnitCommand with ConnectionCommand {
    val encoded: Encoded = encoder("AUTH").optAdd(username).add(password).result
  }

  private object Quit extends RedisUnitCommand with ConnectionCommand {
    val encoded: Encoded = encoder("QUIT").result
  }

  private final class Select(index: Int) extends RedisUnitCommand with ConnectionCommand {
    val encoded: Encoded = encoder("SELECT").add(index).result
  }
}

