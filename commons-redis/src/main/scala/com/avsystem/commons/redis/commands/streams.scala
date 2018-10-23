package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.{AbstractRedisCommand, ApiSubset, NodeCommand, RedisBooleanCommand, RedisIntCommand, RedisLongCommand, RedisUnitCommand}

trait StreamsApi extends ApiSubset {
  private final class Xack(key: Key, group: XGroup, ids: Iterable[XEntryId])
    extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("XACK").key(key).add(group).add(ids).result

    override def immediateResult: Opt[Int] = whenEmpty(ids, 0)
  }

  private final class Xadd(key: Key, maxlen: Opt[XMaxlen], id: Opt[XEntryId], fieldValues: Iterable[(Field, Value)])
    extends AbstractRedisCommand[XEntryId](bulkXEntryId) with NodeCommand {
    val encoded: Encoded = encoder("XADD").key(key).optAdd("MAXLEN", maxlen)
      .add(id.fold("*")(_.toString)).dataPairs(fieldValues).result
  }

  private abstract class AbstractXclaim[A](entryDecoder: ReplyDecoder[A])(
    key: Key, group: XGroup, consumer: XConsumer, minIdleTime: Long, ids: Iterable[XEntryId],
    idle: Opt[Long], msUnixTime: Opt[Long], retrycount: Opt[Int], force: Boolean, justid: Boolean
  ) extends AbstractRedisCommand[Seq[A]](multiBulkSeq(entryDecoder)) with NodeCommand {
    val encoded: Encoded = encoder("XCLAIM").key(key).add(group).add(consumer).add(minIdleTime)
      .add(ids).optAdd("IDLE", idle).optAdd("TIME", msUnixTime).optAdd("RETRYCOUNT", retrycount)
      .addFlag("FORCE", force).addFlag("JUSTID", justid).result

    override def immediateResult: Opt[Seq[A]] = whenEmpty(ids, Seq.empty)
  }

  private final class Xclaim(
    key: Key, group: XGroup, consumer: XConsumer, minIdleTime: Long, ids: Iterable[XEntryId],
    idle: Opt[Long], msUnixTime: Opt[Long], retrycount: Opt[Int], force: Boolean
  ) extends AbstractXclaim[(XEntryId, BMap[Field, Value])](multiBulkPair(bulkXEntryId, mapMultiBulk[Field, Value]))(
    key, group, consumer, minIdleTime, ids, idle, msUnixTime, retrycount, force, justid = false)

  private final class XclaimJustid(
    key: Key, group: XGroup, consumer: XConsumer, minIdleTime: Long, ids: Iterable[XEntryId],
    idle: Opt[Long], msUnixTime: Opt[Long], retrycount: Opt[Int], force: Boolean
  ) extends AbstractXclaim[XEntryId](bulkXEntryId)(
    key, group, consumer, minIdleTime, ids, idle, msUnixTime, retrycount, force, justid = true)

  private final class Xdel(key: Key, ids: Iterable[XEntryId]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("XDEL").key(key).add(ids).result

    override def immediateResult: Opt[Long] = whenEmpty(ids, 0)
  }

  private final class XgroupCreate(key: Key, group: XGroup, id: Opt[XEntryId], mkstream: Boolean)
    extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("XGROUP", "CREATE").key(key).add(group)
      .add(id.fold("$")(_.toString)).addFlag("MKSTREAM", mkstream).result
  }

  private final class XgroupSetid(key: Key, group: XGroup, id: Opt[XEntryId])
    extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("XGROUP", "SETID").key(key).add(group).add(id.fold("$")(_.toString)).result
  }

  private final class XgroupDestroy(key: Key, group: XGroup)
    extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("XGROUP", "DESTROY").key(key).add(group).result
  }

  private final class XgroupDelconsumer(key: Key, group: XGroup, consumer: XConsumer)
    extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("XGROUP", "DELCONSUMER").key(key).add(group).add(consumer).result
  }

  // TODO: XINFO

  private final class Xlen(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("XLEN").key(key).result
  }
}

final case class XEntryId(tstamp: Long, seq: OptArg[Long] = OptArg.Empty) {
  override def toString: String = s"$tstamp${seq.fold("")("-" + _)}"
}
object XEntryId {
  final val Zero: XEntryId = XEntryId(0)

  def parse(str: String): XEntryId = str.indexOf('-') match {
    case -1 => XEntryId(str.toLong, OptArg.Empty)
    case i => XEntryId(str.substring(0, i).toLong, OptArg(str.substring(i + 1).toLong))
  }

  implicit val commandArg: CommandArg[XEntryId] = CommandArg((enc, eid) => enc.add(eid.toString))
}

case class XMaxlen(maxlen: Long, approx: Boolean = true)
object XMaxlen {
  implicit val commandArg: CommandArg[XMaxlen] = CommandArg {
    case (enc, XMaxlen(maxlen, approx)) => enc.addFlag("~", approx).add(maxlen)
  }
}

final case class XGroup(raw: String) extends AnyVal
object XGroup {
  implicit val commandArg: CommandArg[XGroup] = CommandArg((e, v) => e.add(v.raw))
}

final case class XConsumer(raw: String) extends AnyVal
object XConsumer {
  implicit val commandArg: CommandArg[XConsumer] = CommandArg((e, v) => e.add(v.raw))
}
