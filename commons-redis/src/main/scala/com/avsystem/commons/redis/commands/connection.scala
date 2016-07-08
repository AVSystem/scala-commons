package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis.Scope.{Connection, Node}
import com.avsystem.commons.redis.{ConnectionApiSubset, NodeApiSubset, RedisBinaryCommand, RedisSimpleStringCommand, RedisUnitCommand}

trait NodeConnectionApi extends NodeApiSubset {
  def echo(message: ByteString): Result[ByteString, Node] =
    execute(Echo(message))

  def ping: Result[ByteString, Node] =
    execute(Ping)
}

trait ConnectionConnectionApi extends ConnectionApiSubset with NodeConnectionApi {
  def auth(password: ByteString): Result[Unit, Connection] =
    execute(Auth(password))

  def quit: Result[Unit, Connection] =
    execute(Quit)

  def select(index: Int): Result[Unit, Connection] =
    execute(Select(index))
}

case class Auth(password: ByteString) extends RedisUnitCommand[Connection] {
  def encode = encoder("AUTH").add(password).result
}

case class Echo(message: ByteString) extends RedisBinaryCommand[Node] {
  def encode = encoder("ECHO").add(message).result
}

case object Ping extends RedisSimpleStringCommand[Node] {
  def encode = encoder("PING").result
}

case object Quit extends RedisUnitCommand[Connection] {
  def encode = encoder("QUIT").result
}

case class Select(index: Int) extends RedisUnitCommand[Connection] {
  def encode = encoder("SELECT").add(index).result
}
