package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait NodeConnectionApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/echo ECHO]] */
  def echo(message: Value): Result[Value] =
    execute(new Echo(message))
  /** Executes [[http://redis.io/commands/ping PING]] */
  def ping: Result[ByteString] =
    execute(Ping)

  private final class Echo(message: Value) extends RedisDataCommand[Value] with NodeCommand {
    val encoded = encoder("ECHO").data(message).result
  }

  private object Ping extends AbstractRedisCommand[ByteString](simpleBinary) with NodeCommand {
    val encoded = encoder("PING").result
  }
}

trait ConnectionConnectionApi extends NodeConnectionApi {
  /** Executes [[http://redis.io/commands/auth AUTH]] */
  def auth(password: String): Result[Unit] =
    execute(new Auth(password))
  /** Executes [[http://redis.io/commands/quit QUIT]] */
  def quit: Result[Unit] =
    execute(Quit)
  /** Executes [[http://redis.io/commands/select SELECT]] */
  def select(index: Int): Result[Unit] =
    execute(new Select(index))

  private final class Auth(password: String) extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("AUTH").add(password).result
  }

  private object Quit extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("QUIT").result
  }

  private final class Select(index: Int) extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("SELECT").add(index).result
  }
}

