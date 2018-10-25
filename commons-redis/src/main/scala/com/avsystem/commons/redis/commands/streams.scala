package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.{AbstractRedisCommand, ApiSubset, NodeCommand, RedisBooleanCommand, RedisIntCommand, RedisLongCommand, RedisSeqCommand, RedisUnitCommand}

trait StreamsApi extends ApiSubset {
  private final class Xack(key: Key, group: XGroup, ids: Iterable[XEntryId])
    extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("XACK").key(key).add(group).add(ids).result

    override def immediateResult: Opt[Int] = whenEmpty(ids, 0)
  }

  private final class Xadd(key: Key, maxlen: Opt[XMaxlen], id: Opt[XEntryId], fieldValues: Iterable[(Field, Value)])
    extends AbstractRedisCommand[XEntryId](bulkXEntryId) with NodeCommand {
    val encoded: Encoded = encoder("XADD").key(key).optAdd("MAXLEN", maxlen)
      .optAdd(id, "*").dataPairs(fieldValues).result
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
      .optAdd(id, "$").addFlag("MKSTREAM", mkstream).result
  }

  private final class XgroupSetid(key: Key, group: XGroup, id: Opt[XEntryId])
    extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("XGROUP", "SETID").key(key).add(group).optAdd(id, "$").result
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

  private final class Xpending(key: Key, group: XGroup)
    extends AbstractRedisCommand[XPendingOverview](multiBulkXPendingOverview) with NodeCommand {
    val encoded: Encoded = encoder("XPENDING").key(key).add(group).result
  }

  private final class XpendingEntries(key: Key, group: XGroup,
    start: Opt[XEntryId], end: Opt[XEntryId], count: Int, consumer: Opt[XConsumer]
  ) extends RedisSeqCommand[XPendingEntry](multiBulkXPendingEntry) with NodeCommand {
    val encoded: Encoded = encoder("XPENDING").key(key).add(group)
      .optAdd(start, "-").optAdd(end, "+").add(count).optAdd(consumer).result
  }

  private final class Xrange(key: Key, start: Opt[XEntryId], end: Opt[XEntryId], count: Opt[Int])
    extends RedisSeqCommand[(XEntryId, BMap[Field, Value])](multiBulkXEntry[Field, Value]) with NodeCommand {
    val encoded: Encoded = encoder("XRANGE").key(key)
      .optAdd(start, "-").optAdd(end, "+").optAdd("COUNT", count).result
  }

  private abstract class AbstractXread
    extends AbstractRedisCommand[BMap[Key, Seq[(XEntryId, BMap[Field, Value])]]](
      multiBulkXEntriesByKey[Key, Field, Value]) with NodeCommand {

    def streams: Iterable[(Key, _)]
    def blockMillis: Opt[Int]

    override def immediateResult: Opt[BMap[Key, Seq[(XEntryId, BMap[Field, Value])]]] =
      whenEmpty(streams, Map.empty)
    override def maxBlockingMillis: Int =
      blockMillis.map(m => if (m <= 0) Int.MaxValue else m).getOrElse(0)
  }

  private final class Xread(
    count: Opt[Int], val blockMillis: Opt[Int], val streams: Iterable[(Key, XEntryId)]
  ) extends AbstractXread {

    val encoded: Encoded = encoder("XREAD")
      .optAdd("COUNT", count).optAdd("BLOCK", blockMillis)
      .add("STREAMS").keys(streams.iterator.map(_._1)).add(streams.iterator.map(_._2))
      .result
  }

  private final class Xreadgroup(
    group: XGroup, consumer: XConsumer,
    count: Opt[Int], val blockMillis: Opt[Int], val streams: Iterable[(Key, Opt[XEntryId])]
  ) extends AbstractXread {

    val encoded: Encoded = encoder("XREADGROUP").add("GROUP").add(group).add(consumer)
      .optAdd("COUNT", count).optAdd("BLOCK", blockMillis)
      .add("STREAMS").keys(streams.iterator.map(_._1)).add(streams.iterator.map(_._2.fold(">")(_.toString)))
      .result
  }

  private final class Xrevrange(key: Key, end: Opt[XEntryId], start: Opt[XEntryId], count: Opt[Int])
    extends RedisSeqCommand[(XEntryId, BMap[Field, Value])](multiBulkXEntry[Field, Value]) with NodeCommand {
    val encoded: Encoded = encoder("XREVRANGE").key(key)
      .optAdd(end, "+").optAdd(start, "-").optAdd("COUNT", count).result
  }

  private final class Xtrim(key: Key, maxlen: XMaxlen) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("XTRIM").key(key).add("MAXLEN").add(maxlen).result
  }
}

final case class XEntryId(tstamp: Long, seq: OptArg[Long] = OptArg.Empty) extends Ordered[XEntryId] {
  override def compare(that: XEntryId): Int = {
    val byTstamp = java.lang.Long.compareUnsigned(tstamp, that.tstamp)
    if (byTstamp != 0) byTstamp
    else java.lang.Long.compareUnsigned(seq.getOrElse(0L), that.seq.getOrElse(0L))
  }

  def inc: XEntryId =
    XEntryId(tstamp, seq.getOrElse(0L) + 1)
  def fillMinSeq: XEntryId =
    if (seq.isDefined) this else XEntryId(tstamp, 0L)
  def fillMaxSeq: XEntryId =
    if (seq.isDefined) this else XEntryId(tstamp, Long.MinValue) // unsigned!

  private def ultostr(ul: Long): String =
    java.lang.Long.toUnsignedString(ul)

  override def toString: String =
    s"${ultostr(tstamp)}${seq.fold("")(v => "-" + ultostr(v))}"
}
object XEntryId {
  final val Zero: XEntryId = XEntryId(0)

  private def strtoul(str: String): Long =
    java.lang.Long.parseUnsignedLong(str)

  def parse(str: String): XEntryId = str.indexOf('-') match {
    case -1 => XEntryId(strtoul(str), OptArg.Empty)
    case i => XEntryId(strtoul(str.substring(0, i)), OptArg(strtoul(str.substring(i + 1))))
  }

  implicit val ordering: Ordering[XEntryId] = new Ordering[XEntryId] {
    def compare(x: XEntryId, y: XEntryId): Int = x.compare(y)
  }

  implicit val commandArg: CommandArg[XEntryId] =
    CommandArg((enc, eid) => enc.add(eid.toString))
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

case class XPendingOverview(
  count: Long,
  minId: XEntryId,
  maxId: XEntryId,
  countByConsumer: BMap[XConsumer, Long]
)
object XPendingOverview {
  final val Empty = XPendingOverview(0, XEntryId.Zero, XEntryId.Zero, Map.empty)
}

case class XPendingEntry(
  id: XEntryId,
  consumer: XConsumer,
  idleTime: Long,
  deliveredCount: Int
)
