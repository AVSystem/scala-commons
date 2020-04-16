package com.avsystem.commons.redis.commands

import com.avsystem.commons.redis.UnsafeCommand
import com.avsystem.commons.redis.protocol.ValidRedisMsg

private[redis] sealed abstract class PubSubCommand extends UnsafeCommand

private[redis] final class Subscribe(channels: Iterable[String]) extends PubSubCommand {
  require(channels.nonEmpty, "at least one channel must be specified")
  val encoded: Encoded = encoder("SUBSCRIBE").add(channels).result
}

private[redis] final class Psubscribe(patterns: Iterable[String]) extends PubSubCommand {
  require(patterns.nonEmpty, "at least one pattern must be specified")
  val encoded: Encoded = encoder("PSUBSCRIBE").add(patterns).result
}

private[redis] final class Unsubscribe(channels: Iterable[String]) extends PubSubCommand {
  require(channels.nonEmpty, "at least one channel must be specified")
  val encoded: Encoded = encoder("UNSUBSCRIBE").add(channels).result
}

private[redis] final class Punsubscribe(patterns: Iterable[String]) extends PubSubCommand {
  require(patterns.nonEmpty, "at least one pattern must be specified")
  val encoded: Encoded = encoder("PUNSUBSCRIBE").add(patterns).result
}

sealed abstract class PubSubEvent
sealed abstract class PubSubChannelEvent extends PubSubEvent {
  def channel: String
}
object PubSubEvent {
  final case class Subscribe(channel: String, subscribed: Int) extends PubSubChannelEvent
  final case class Unsubscribe(channel: String, subscribed: Int) extends PubSubChannelEvent
  final case class Message(channel: String, message: ValidRedisMsg) extends PubSubChannelEvent
  case object ConnectionLost extends PubSubEvent
}
