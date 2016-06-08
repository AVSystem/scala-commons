package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis.{RedisRawCommand, RedisUnitCommand, Scope}

case class Watch(keys: Seq[ByteString]) extends RedisUnitCommand[Scope] {
  def encode = encoder("WATCH").add(keys).result
  def isKey(idx: Int) = idx > 0
}

case object Unwatch extends RedisUnitCommand[Scope] {
  def encode = encoder("UNWATCH").result
  def isKey(idx: Int) = false
}

case object Multi extends RedisUnitCommand[Scope] {
  def encode = encoder("MULTI").result
  def isKey(idx: Int) = false
}

case object Exec extends RedisRawCommand[Scope] {
  def encode = encoder("EXEC").result
  def isKey(idx: Int) = false
}

case object Discard extends RedisUnitCommand[Scope] {
  def encode = encoder("DISCARD").result
  def isKey(idx: Int) = false
}
