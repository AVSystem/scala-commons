package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait NodeConnectionApi extends ApiSubset {
  def echo(message: ByteString): Result[ByteString] =
    execute(new Echo(message))

  def ping: Result[ByteString] =
    execute(Ping)

  private final class Echo(message: ByteString) extends RedisBinaryCommand with NodeCommand {
    val encoded = encoder("ECHO").add(message).result
  }

  private object Ping extends AbstractRedisCommand[ByteString](simpleBinary) with NodeCommand {
    val encoded = encoder("PING").result
  }
}

trait ConnectionConnectionApi extends NodeConnectionApi {
  def auth(password: String): Result[Unit] =
    execute(new Auth(password))

  def quit: Result[Unit] =
    execute(Quit)

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

