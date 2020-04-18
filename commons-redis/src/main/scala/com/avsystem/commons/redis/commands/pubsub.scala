package com.avsystem.commons.redis.commands

import akka.util.ByteString
import com.avsystem.commons.redis.UnsafeCommand
import com.avsystem.commons.redis.protocol.{BulkStringMsg, ValidRedisMsg}

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
object PubSubEvent {
  final case class Subscribe(channel: String, subscribed: Int) extends PubSubEvent
  final case class Psubscribe(pattern: String, subscribed: Int) extends PubSubEvent
  final case class Unsubscribe(channel: String, subscribed: Int) extends PubSubEvent
  final case class Punsubscribe(pattern: String, subscribed: Int) extends PubSubEvent
  final case class Message(channel: String, message: ValidRedisMsg) extends PubSubEvent
  final case class Pmessage(pattern: String, channel: String, message: ValidRedisMsg) extends PubSubEvent
  case object ConnectionLost extends PubSubEvent

  final val MessageStr = BulkStringMsg(ByteString("message"))
  final val PmessageStr = BulkStringMsg(ByteString("pmessage"))
  final val SubscribeStr = BulkStringMsg(ByteString("subscribe"))
  final val PsubscribeStr = BulkStringMsg(ByteString("psubscribe"))
  final val UnsubscribeStr = BulkStringMsg(ByteString("unsubscribe"))
  final val PunsubscribeStr = BulkStringMsg(ByteString("punsubscribe"))
}
