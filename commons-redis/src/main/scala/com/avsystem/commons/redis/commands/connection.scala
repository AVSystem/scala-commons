package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis._

trait NodeConnectionApi extends ApiSubset {
  def echo(message: ByteString): Result[ByteString] =
    execute(Echo(message))

  def ping: Result[ByteString] =
    execute(Ping)

  private case class Echo(message: ByteString) extends RedisBinaryCommand with NodeCommand {
    val encoded = encoder("ECHO").add(message).result
  }

  private case object Ping extends RedisSimpleStringCommand with NodeCommand {
    val encoded = encoder("PING").result
  }
}

trait ConnectionConnectionApi extends NodeConnectionApi {
  def auth(password: ByteString): Result[Unit] =
    execute(Auth(password))

  def quit: Result[Unit] =
    execute(Quit)

  def select(index: Int): Result[Unit] =
    execute(Select(index))

  private case class Auth(password: ByteString) extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("AUTH").add(password).result
  }

  private case object Quit extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("QUIT").result
  }

  private case class Select(index: Int) extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("SELECT").add(index).result
  }
}

