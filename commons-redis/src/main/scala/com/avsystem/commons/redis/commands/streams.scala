package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.protocol.ValidRedisMsg
import com.avsystem.commons.redis.util.SingletonSeq

trait StreamsApi extends ApiSubset {
  type XEntry = redis.commands.XEntry[Record]
  object XEntry {
    def apply(id: XEntryId, data: Record): XEntry = redis.commands.XEntry(id, data)
    def unapply(entry: XEntry): Opt[(XEntryId, Record)] = Opt((entry.id, entry.data))
  }

  /** Executes [[http://redis.io/commands/xack XACK]] */
  def xack(key: Key, group: XGroup, id: XEntryId): Result[Boolean] =
    execute(new Xack(key, group, new SingletonSeq(id)).map(_ > 0))

  /** Executes [[http://redis.io/commands/xack XACK]] */
  def xack(key: Key, group: XGroup, id: XEntryId, ids: XEntryId*): Result[Int] =
    execute(new Xack(key, group, id +:: ids))

  /** Executes [[http://redis.io/commands/xack XACK]] */
  def xack(key: Key, group: XGroup, ids: Iterable[XEntryId]): Result[Int] =
    execute(new Xack(key, group, ids))

  /** Executes [[http://redis.io/commands/xadd XADD]] */
  def xadd(
    key: Key,
    data: Record,
    id: OptArg[XEntryId] = OptArg.Empty,
    maxlen: OptArg[XMaxlen] = OptArg.Empty
  ): Result[XEntryId] =
    execute(new Xadd(key, maxlen.toOpt, id.toOpt, data))

  /** Executes [[http://redis.io/commands/xadd XADD]] */
  def xaddEntry(key: Key, entry: XEntry, maxlen: OptArg[XMaxlen] = OptArg.Empty): Result[XEntryId] =
    execute(new Xadd(key, maxlen.toOpt, entry.id.opt, entry.data))

  /** Executes [[http://redis.io/commands/xclaim XCLAIM]] */
  def xclaimSingle(
    key: Key,
    group: XGroup,
    consumer: XConsumer,
    minIdleMillis: Long,
    id: XEntryId,
    idleMillis: OptArg[Long] = OptArg.Empty,
    msUnixTime: OptArg[Long] = OptArg.Empty,
    retrycount: OptArg[Int] = OptArg.Empty,
    force: Boolean = false
  ): Result[Opt[XEntry]] =
    execute(new Xclaim(
      key, group, consumer, minIdleMillis, new SingletonSeq(id),
      idleMillis.toOpt, msUnixTime.toOpt, retrycount.toOpt, force
    ).map(_.headOpt))

  /** Executes [[http://redis.io/commands/xclaim XCLAIM]] */
  def xclaim(
    key: Key,
    group: XGroup,
    consumer: XConsumer,
    minIdleMillis: Long,
    ids: Iterable[XEntryId],
    idleMillis: OptArg[Long] = OptArg.Empty,
    msUnixTime: OptArg[Long] = OptArg.Empty,
    retrycount: OptArg[Int] = OptArg.Empty,
    force: Boolean = false
  ): Result[Seq[XEntry]] =
    execute(new Xclaim(
      key, group, consumer, minIdleMillis, ids,
      idleMillis.toOpt, msUnixTime.toOpt, retrycount.toOpt, force
    ))

  /** Executes [[http://redis.io/commands/xclaim XCLAIM]] */
  def xclaimJustid(
    key: Key,
    group: XGroup,
    consumer: XConsumer,
    minIdleMillis: Long,
    ids: Iterable[XEntryId],
    idleMillis: OptArg[Long] = OptArg.Empty,
    msUnixTime: OptArg[Long] = OptArg.Empty,
    retrycount: OptArg[Int] = OptArg.Empty,
    force: Boolean = false
  ): Result[Seq[XEntryId]] =
    execute(new XclaimJustid(
      key, group, consumer, minIdleMillis, ids,
      idleMillis.toOpt, msUnixTime.toOpt, retrycount.toOpt, force
    ))

  /** Executes [[http://redis.io/commands/xdel XDEL]] */
  def xdel(key: Key, id: XEntryId): Result[Boolean] =
    execute(new Xdel(key, new SingletonSeq(id)).map(_ > 0))

  /** Executes [[http://redis.io/commands/xdel XDEL]] */
  def xdel(key: Key, id: XEntryId, ids: XEntryId*): Result[Long] =
    execute(new Xdel(key, id +:: ids))

  /** Executes [[http://redis.io/commands/xdel XDEL]] */
  def xdel(key: Key, ids: Iterable[XEntryId]): Result[Long] =
    execute(new Xdel(key, ids))

  /** Executes [[http://redis.io/commands/xgroup XGROUP CREATE]] */
  def xgroupCreate(key: Key, group: XGroup, id: OptArg[XEntryId] = OptArg.Empty, mkstream: Boolean = false): Result[Unit] =
    execute(new XgroupCreate(key, group, id.toOpt, mkstream))

  /** Executes [[http://redis.io/commands/xgroup XGROUP DELCONSUMER]] */
  def xgroupDelconsumer(key: Key, group: XGroup, consumer: XConsumer): Result[Boolean] =
    execute(new XgroupDelconsumer(key, group, consumer))

  /** Executes [[http://redis.io/commands/xgroup XGROUP DESTROY]] */
  def xgroupDestroy(key: Key, group: XGroup): Result[Boolean] =
    execute(new XgroupDestroy(key, group))

  /** Executes [[http://redis.io/commands/xgroup XGROUP SETID]] */
  def xgroupSetid(key: Key, group: XGroup, id: OptArg[XEntryId] = OptArg.Empty): Result[Unit] =
    execute(new XgroupSetid(key, group, id.toOpt))

  /** Executes [[http://redis.io/commands/xinfo XINFO CONSUMERS]] */
  def xinfoConsumers(key: Key, group: XGroup): Result[Seq[XConsumerInfo]] =
    execute(new XinfoConsumers(key, group))

  /** Executes [[http://redis.io/commands/xinfo XINFO GROUPS]] */
  def xinfoGroups(key: Key): Result[Seq[XGroupInfo]] =
    execute(new XinfoGroups(key))

  /** Executes [[http://redis.io/commands/xinfo XINFO STREAM]] */
  def xinfoStream(key: Key): Result[XStreamInfo[Record]] =
    execute(new XinfoStream(key))

  /** Executes [[http://redis.io/commands/xlen XLEN]] */
  def xlen(key: Key): Result[Long] =
    execute(new Xlen(key))

  /** Executes [[http://redis.io/commands/xpending XPENDING]] */
  def xpending(key: Key, group: XGroup): Result[XPendingOverview] =
    execute(new Xpending(key, group))

  /** Executes [[http://redis.io/commands/xpending XPENDING]] */
  def xpendingEntries(
    key: Key,
    group: XGroup,
    count: Int,
    start: OptArg[XEntryId] = OptArg.Empty,
    end: OptArg[XEntryId] = OptArg.Empty,
    consumer: OptArg[XConsumer] = OptArg.Empty
  ): Result[Seq[XPendingEntry]] =
    execute(new XpendingEntries(key, group, start.toOpt, end.toOpt, count, consumer.toOpt))

  /** Executes [[http://redis.io/commands/xrange XRANGE]] */
  def xrange(
    key: Key,
    start: OptArg[XEntryId] = OptArg.Empty,
    end: OptArg[XEntryId] = OptArg.Empty,
    count: OptArg[Int] = OptArg.Empty
  ): Result[Seq[XEntry]] =
    execute(new Xrange(key, start.toOpt, end.toOpt, count.toOpt))

  /** Executes [[http://redis.io/commands/xread XREAD]] */
  def xreadSingle(
    key: Key,
    id: Opt[XEntryId],
    blockMillis: OptArg[Int] = OptArg.Empty,
    count: OptArg[Int] = OptArg.Empty
  ): Result[Seq[XEntry]] =
    execute(new Xread(count.toOpt, blockMillis.toOpt, Iterator(key), Iterator(id)).map(_.getOrElse(key, Nil)))

  /** Executes [[http://redis.io/commands/xread XREAD]] */
  def xread(
    streams: Iterable[(Key, Opt[XEntryId])],
    blockMillis: OptArg[Int] = OptArg.Empty,
    count: OptArg[Int] = OptArg.Empty
  ): Result[BMap[Key, Seq[XEntry]]] =
    execute(new Xread(count.toOpt, blockMillis.toOpt, streams.iterator.map(_._1), streams.iterator.map(_._2)))

  /** Executes [[http://redis.io/commands/xreadgroup XREADGROUP]] */
  def xreadgroupSingle(
    key: Key,
    group: XGroup,
    consumer: XConsumer,
    id: OptArg[XEntryId] = OptArg.Empty,
    blockMillis: OptArg[Int] = OptArg.Empty,
    count: OptArg[Int] = OptArg.Empty,
    noack: Boolean = false
  ): Result[Seq[XEntry]] =
    execute(new Xreadgroup(group, consumer, count.toOpt, blockMillis.toOpt, noack,
      Iterator(key), Iterator(id.toOpt)).map(_.getOrElse(key, Nil)))

  /** Executes [[http://redis.io/commands/xreadgroup XREADGROUP]] */
  def xreadgroup(
    group: XGroup,
    consumer: XConsumer,
    streams: Iterable[(Key, Opt[XEntryId])],
    blockMillis: OptArg[Int] = OptArg.Empty,
    count: OptArg[Int] = OptArg.Empty,
    noack: Boolean = false
  ): Result[BMap[Key, Seq[XEntry]]] =
    execute(new Xreadgroup(group, consumer, count.toOpt, blockMillis.toOpt, noack,
      streams.iterator.map(_._1), streams.iterator.map(_._2)))

  /** Executes [[http://redis.io/commands/xrevrange XREVRANGE]] */
  def xrevrange(
    key: Key,
    end: OptArg[XEntryId] = OptArg.Empty,
    start: OptArg[XEntryId] = OptArg.Empty,
    count: OptArg[Int] = OptArg.Empty
  ): Result[Seq[XEntry]] =
    execute(new Xrevrange(key, end.toOpt, start.toOpt, count.toOpt))

  /** Executes [[http://redis.io/commands/xtrim XTRIM]] */
  def xtrim(key: Key, maxlen: Long, approx: Boolean = true): Result[Long] =
    execute(new Xtrim(key, XMaxlen(maxlen, approx)))

  /** Executes [[http://redis.io/commands/xtrim XTRIM]] */
  def xtrim(key: Key, maxlen: XMaxlen): Result[Long] =
    execute(new Xtrim(key, maxlen))

  private final class Xack(key: Key, group: XGroup, ids: Iterable[XEntryId])
    extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("XACK").key(key).add(group).add(ids).result

    override def immediateResult: Opt[Int] = whenEmpty(ids, 0)
  }

  private final class Xadd(key: Key, maxlen: Opt[XMaxlen], id: Opt[XEntryId], data: Record)
    extends AbstractRedisCommand[XEntryId](bulkXEntryId) with NodeCommand {
    val encoded: Encoded = encoder("XADD").key(key).optAdd("MAXLEN", maxlen)
      .optAdd(id, "*").dataPairs(data).result
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
  ) extends AbstractXclaim[XEntry](multiBulkXEntry)(
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

  private final class XgroupDelconsumer(key: Key, group: XGroup, consumer: XConsumer)
    extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("XGROUP", "DELCONSUMER").key(key).add(group).add(consumer).result
  }

  private final class XgroupDestroy(key: Key, group: XGroup)
    extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("XGROUP", "DESTROY").key(key).add(group).result
  }

  private final class XgroupSetid(key: Key, group: XGroup, id: Opt[XEntryId])
    extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("XGROUP", "SETID").key(key).add(group).optAdd(id, "$").result
  }

  private final class XinfoConsumers(key: Key, group: XGroup)
    extends RedisSeqCommand[XConsumerInfo](multiBulkXConsumerInfo) with NodeCommand {
    val encoded: Encoded = encoder("XINFO", "CONSUMERS").key(key).add(group).result
  }

  private final class XinfoGroups(key: Key)
    extends RedisSeqCommand[XGroupInfo](multiBulkXGroupInfo) with NodeCommand {
    val encoded: Encoded = encoder("XINFO", "GROUPS").key(key).result
  }

  private final class XinfoStream(key: Key)
    extends AbstractRedisCommand[XStreamInfo[Record]](multiBulkXStreamInfo[Record]) with NodeCommand {
    val encoded: Encoded = encoder("XINFO", "STREAM").key(key).result
  }

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
    extends RedisSeqCommand[XEntry](multiBulkXEntry[Record]) with NodeCommand {
    val encoded: Encoded = encoder("XRANGE").key(key)
      .optAdd(start, "-").optAdd(end, "+").optAdd("COUNT", count).result
  }

  private abstract class AbstractXread(noStreams: Boolean)
    extends AbstractRedisCommand[BMap[Key, Seq[XEntry]]](
      multiBulkXEntriesMap[Key, Record]) with NodeCommand {

    def blockMillis: Opt[Int]

    override def immediateResult: Opt[BMap[Key, Seq[XEntry]]] =
      if (noStreams) Opt(Map.empty) else Opt.Empty
    override def maxBlockingMillis: Int =
      blockMillis.map(m => if (m <= 0) Int.MaxValue else m).getOrElse(0)
  }

  private final class Xread(
    count: Opt[Int], val blockMillis: Opt[Int],
    streamKeys: Iterator[Key], streamIds: Iterator[Opt[XEntryId]]
  ) extends AbstractXread(streamKeys.isEmpty) {
    val encoded: Encoded = encoder("XREAD")
      .optAdd("COUNT", count).optAdd("BLOCK", blockMillis)
      .add("STREAMS").keys(streamKeys).add(streamIds.map(_.fold("$")(_.toString)))
      .result
  }

  private final class Xreadgroup(
    group: XGroup, consumer: XConsumer,
    count: Opt[Int], val blockMillis: Opt[Int], noack: Boolean,
    streamKeys: Iterator[Key], streamIds: Iterator[Opt[XEntryId]]
  ) extends AbstractXread(streamKeys.isEmpty) {
    val encoded: Encoded = encoder("XREADGROUP").add("GROUP").add(group).add(consumer)
      .optAdd("COUNT", count).optAdd("BLOCK", blockMillis).addFlag("NOACK", noack)
      .add("STREAMS").keys(streamKeys).add(streamIds.map(_.fold(">")(_.toString)))
      .result
  }

  private final class Xrevrange(key: Key, end: Opt[XEntryId], start: Opt[XEntryId], count: Opt[Int])
    extends RedisSeqCommand[XEntry](multiBulkXEntry[Record]) with NodeCommand {
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

case class XEntry[Record](id: XEntryId, data: Record)

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

case class XGroupInfo(raw: BMap[String, ValidRedisMsg]) {
  def name: XGroup = bulkXGroup(raw("name"))
  def consumers: Int = integerInt(raw("consumers"))
  def pending: Int = integerInt(raw("pending"))
  def lastDeliveredId: XEntryId = bulkXEntryId(raw("last-delivered-id"))
}

case class XConsumerInfo(raw: BMap[String, ValidRedisMsg]) {
  def name: XConsumer = bulkXConsumer(raw("name"))
  def pending: Int = integerInt(raw("pending"))
  def idle: Long = integerLong(raw("idle"))
}

case class XStreamInfo[Record: RedisRecordCodec](raw: BMap[String, ValidRedisMsg]) {
  def length: Long = integerLong(raw("length"))
  def radixTreeKeys: Int = integerInt(raw("radis-tree-keys"))
  def radixTreeNodes: Int = integerInt(raw("radis-tree-nodes"))
  def groups: Int = integerInt(raw("groups"))
  def lastGeneratedId: XEntryId = bulkXEntryId(raw("last-generated-id"))
  def firstEntry: XEntry[Record] = ReplyDecoders.multiBulkXEntry[Record].apply(raw("first-entry"))
  def lastEntry: XEntry[Record] = ReplyDecoders.multiBulkXEntry[Record].apply(raw("last-entry"))
}
