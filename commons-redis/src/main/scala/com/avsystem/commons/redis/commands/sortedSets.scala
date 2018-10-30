package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.util.SingletonSeq

trait SortedSetsApi extends ValueApiSubset {
  /** Executes [[http://redis.io/commands/zadd ZADD]] */
  def zadd(key: Key, memberScore: (Value, Double), memberScores: (Value, Double)*): Result[Int] =
    zadd(key, memberScore +:: memberScores)
  /** Executes [[http://redis.io/commands/zadd ZADD]] */
  def zadd(key: Key, member: Value, score: Double): Result[Boolean] =
    execute(new Zadd(key, (member, score).single, emptyData = false, Opt.Empty, changed = false).map(_ > 0))
  /** Executes [[http://redis.io/commands/zadd ZADD]] */
  def zadd(key: Key, score: Double, member: Value, members: Value*): Result[Int] =
    execute(new Zadd(key, (member, score) +:: members.iterator.map((_, score)), emptyData = false, Opt.Empty, changed = false))
  /** Executes [[http://redis.io/commands/zadd ZADD]]
    * or simply returns 0 when `members` is empty */
  def zadd(key: Key, score: Double, members: Iterable[Value]): Result[Int] =
    execute(new Zadd(key, members.iterator.map((_, score)), members.isEmpty, Opt.Empty, changed = false))
  /** Executes [[http://redis.io/commands/zadd ZADD]]
    * or simply returns 0 when `memberScores` is empty */
  def zadd(key: Key, memberScores: Iterable[(Value, Double)], existence: OptArg[Boolean] = OptArg.Empty, changed: Boolean = false): Result[Int] =
    execute(new Zadd(key, memberScores, memberScores.isEmpty, existence.toOpt, changed))
  /** Executes [[http://redis.io/commands/zadd ZADD]] */
  def zaddIncr(key: Key, member: Value, score: Double, existence: OptArg[Boolean] = OptArg.Empty): Result[Opt[Double]] =
    execute(new ZaddIncr(key, member, score, existence.toOpt))
  /** Executes [[http://redis.io/commands/zcard ZCARD]] */
  def zcard(key: Key): Result[Long] =
    execute(new Zcard(key))
  /** Executes [[http://redis.io/commands/zcount ZCOUNT]] */
  def zcount(key: Key, min: ScoreLimit = ScoreLimit.MinusInf, max: ScoreLimit = ScoreLimit.PlusInf): Result[Long] =
    execute(new Zcount(key, min, max))
  /** Executes [[http://redis.io/commands/zincrby ZINCRBY]] */
  def zincrby(key: Key, increment: Double, member: Value): Result[Double] =
    execute(new Zincrby(key, increment, member))
  /** Executes [[http://redis.io/commands/zinterstore ZINTERSTORE]] */
  def zinterstore(destination: Key, key: Key, keys: Key*): Result[Long] = zinterstore(destination, key +:: keys)
  /** Executes [[http://redis.io/commands/zinterstore ZINTERSTORE]]
    * NOTE: `keys` MUST NOT be empty */
  def zinterstore(destination: Key, keys: Iterable[Key], aggregation: OptArg[Aggregation] = OptArg.Empty): Result[Long] =
    execute(new Zinterstore(destination, keys, Opt.Empty, aggregation.toOpt))
  /** Executes [[http://redis.io/commands/zinterstore ZINTERSTORE]] */
  def zinterstoreWeights(destination: Key, keyWeight: (Key, Double), keysWeights: (Key, Double)*): Result[Long] =
    zinterstoreWeights(destination, keyWeight +:: keysWeights)
  /** Executes [[http://redis.io/commands/zinterstore ZINTERSTORE]]
    * NOTE: `keysWeights` MUST NOT be empty */
  def zinterstoreWeights(destination: Key, keysWeights: Iterable[(Key, Double)], aggregation: OptArg[Aggregation] = OptArg.Empty): Result[Long] =
    execute(new Zinterstore(destination, keysWeights.map(_._1), keysWeights.map(_._2).opt, aggregation.toOpt))
  /** Executes [[http://redis.io/commands/zlexcount ZLEXCOUNT]] */
  def zlexcount(key: Key, min: LexLimit[Value] = LexLimit.MinusInf, max: LexLimit[Value] = LexLimit.PlusInf): Result[Long] =
    execute(new Zlexcount(key, min, max))
  /** Executes [[http://redis.io/commands/zpopmax ZPOPMAX]] */
  def zpopmax(key: Key): Result[Opt[(Value, Double)]] =
    execute(new Zpopmax(key, Opt.Empty).map(_.headOpt))
  /** Executes [[http://redis.io/commands/zpopmax ZPOPMAX]] */
  def zpopmax(key: Key, count: Long): Result[Seq[(Value, Double)]] =
    execute(new Zpopmax(key, Opt(count)))
  /** Executes [[http://redis.io/commands/zpopmin ZPOPMIN]]] */
  def zpopmin(key: Key): Result[Opt[(Value, Double)]] =
    execute(new Zpopmin(key, Opt.Empty).map(_.headOpt))
  /** Executes [[http://redis.io/commands/zpopmin ZPOPMIN]] */
  def zpopmin(key: Key, count: Long): Result[Seq[(Value, Double)]] =
    execute(new Zpopmin(key, Opt(count)))
  /** Executes [[http://redis.io/commands/zrange ZRANGE]] */
  def zrange(key: Key, start: Long = 0, stop: Long = -1): Result[Seq[Value]] =
    execute(new Zrange(key, start, stop))
  /** Executes [[http://redis.io/commands/zrange ZRANGE]] */
  def zrangeWithscores(key: Key, start: Long = 0, stop: Long = -1): Result[Seq[(Value, Double)]] =
    execute(new ZrangeWithscores(key, start, stop))
  /** Executes [[http://redis.io/commands/zrangebylex ZRANGEBYLEX]] */
  def zrangebylex(key: Key, min: LexLimit[Value] = LexLimit.MinusInf, max: LexLimit[Value] = LexLimit.PlusInf, limit: OptArg[Limit] = OptArg.Empty): Result[Seq[Value]] =
    execute(new Zrangebylex(key, min, max, limit.toOpt))
  /** Executes [[http://redis.io/commands/zrangebyscore ZRANGEBYSCORE]] */
  def zrangebyscore(key: Key, min: ScoreLimit = ScoreLimit.MinusInf, max: ScoreLimit = ScoreLimit.PlusInf, limit: OptArg[Limit] = OptArg.Empty): Result[Seq[Value]] =
    execute(new Zrangebyscore(key, min, max, limit.toOpt))
  /** Executes [[http://redis.io/commands/zrangebyscore ZRANGEBYSCORE]] */
  def zrangebyscoreWithscores(key: Key, min: ScoreLimit = ScoreLimit.MinusInf, max: ScoreLimit = ScoreLimit.PlusInf, limit: OptArg[Limit] = OptArg.Empty): Result[Seq[(Value, Double)]] =
    execute(new ZrangebyscoreWithscores(key, min, max, limit.toOpt))
  /** Executes [[http://redis.io/commands/zrank ZRANK]] */
  def zrank(key: Key, member: Value): Result[Opt[Long]] =
    execute(new Zrank(key, member))
  /** Executes [[http://redis.io/commands/zrem ZREM]] */
  def zrem(key: Key, member: Value): Result[Boolean] =
    execute(new Zrem(key, member.single).map(_ > 0))
  /** Executes [[http://redis.io/commands/zrem ZREM]] */
  def zrem(key: Key, member: Value, members: Value*): Result[Int] =
    execute(new Zrem(key, member +:: members))
  /** Executes [[http://redis.io/commands/zrem ZREM]]
    * or simply returns 0 when `members` is empty */
  def zrem(key: Key, members: Iterable[Value]): Result[Int] =
    execute(new Zrem(key, members))
  /** Executes [[http://redis.io/commands/zremrangebylex ZREMRANGEBYLEX]] */
  def zremrangebylex(key: Key, min: LexLimit[Value] = LexLimit.MinusInf, max: LexLimit[Value] = LexLimit.PlusInf): Result[Long] =
    execute(new Zremrangebylex(key, min, max))
  /** Executes [[http://redis.io/commands/zremrangebyrank ZREMRANGEBYRANK]] */
  def zremrangebyrank(key: Key, start: Long = 0, stop: Long = -1): Result[Long] =
    execute(new Zremrangebyrank(key, start, stop))
  /** Executes [[http://redis.io/commands/zremrangebyscore ZREMRANGEBYSCORE]] */
  def zremrangebyscore(key: Key, min: ScoreLimit = ScoreLimit.MinusInf, max: ScoreLimit = ScoreLimit.PlusInf): Result[Long] =
    execute(new Zremrangebyscore(key, min, max))
  /** Executes [[http://redis.io/commands/zrevrange ZREVRANGE]] */
  def zrevrange(key: Key, start: Long = 0, stop: Long = -1): Result[Seq[Value]] =
    execute(new Zrevrange(key, start, stop))
  /** Executes [[http://redis.io/commands/zrevrange ZREVRANGE]] */
  def zrevrangeWithscores(key: Key, start: Long = 0, stop: Long = -1): Result[Seq[(Value, Double)]] =
    execute(new ZrevrangeWithscores(key, start, stop))
  /** Executes [[http://redis.io/commands/zrevrangebylex ZREVRANGEBYLEX]] */
  def zrevrangebylex(key: Key, max: LexLimit[Value] = LexLimit.PlusInf, min: LexLimit[Value] = LexLimit.MinusInf, limit: OptArg[Limit] = OptArg.Empty): Result[Seq[Value]] =
    execute(new Zrevrangebylex(key, max, min, limit.toOpt))
  /** Executes [[http://redis.io/commands/zrevrangebyscore ZREVRANGEBYSCORE]] */
  def zrevrangebyscore(key: Key, max: ScoreLimit = ScoreLimit.PlusInf, min: ScoreLimit = ScoreLimit.MinusInf, limit: OptArg[Limit] = OptArg.Empty): Result[Seq[Value]] =
    execute(new Zrevrangebyscore(key, max, min, limit.toOpt))
  /** Executes [[http://redis.io/commands/zrevrangebyscore ZREVRANGEBYSCORE]] */
  def zrevrangebyscoreWithscores(key: Key, max: ScoreLimit = ScoreLimit.PlusInf, min: ScoreLimit = ScoreLimit.MinusInf, limit: OptArg[Limit] = OptArg.Empty): Result[Seq[(Value, Double)]] =
    execute(new ZrevrangebyscoreWithscores(key, max, min, limit.toOpt))
  /** Executes [[http://redis.io/commands/zrevrank ZREVRANK]] */
  def zrevrank(key: Key, member: Value): Result[Opt[Long]] =
    execute(new Zrevrank(key, member))
  /** Executes [[http://redis.io/commands/zscan ZSCAN]] */
  def zscan(key: Key, cursor: Cursor, matchPattern: OptArg[Value] = OptArg.Empty, count: OptArg[Int] = OptArg.Empty): Result[(Cursor, Seq[(Value, Double)])] =
    execute(new Zscan(key, cursor, matchPattern.toOpt, count.toOpt))
  /** Executes [[http://redis.io/commands/zscore ZSCORE]] */
  def zscore(key: Key, member: Value): Result[Opt[Double]] =
    execute(new Zscore(key, member))
  /** Executes [[http://redis.io/commands/zunionstore ZUNIONSTORE]] */
  def zunionstore(destination: Key, key: Key, keys: Key*): Result[Long] = zunionstore(destination, key +:: keys)
  /** Executes [[http://redis.io/commands/zunionstore ZUNIONSTORE]]
    * NOTE: `keys` MUST NOT be empty */
  def zunionstore(destination: Key, keys: Iterable[Key], aggregation: OptArg[Aggregation] = OptArg.Empty): Result[Long] =
    execute(new Zunionstore(destination, keys, Opt.Empty, aggregation.toOpt))
  /** Executes [[http://redis.io/commands/zunionstore ZUNIONSTORE]] */
  def zunionstoreWeights(destination: Key, keyWeight: (Key, Double), keysWeights: (Key, Double)*): Result[Long] =
    zunionstoreWeights(destination, keyWeight +:: keysWeights)
  /** Executes [[http://redis.io/commands/zunionstore ZUNIONSTORE]]
    * NOTE: `keysWeights` MUST NOT be empty */
  def zunionstoreWeights(destination: Key, keysWeights: Iterable[(Key, Double)], aggregation: OptArg[Aggregation] = OptArg.Empty): Result[Long] =
    execute(new Zunionstore(destination, keysWeights.map(_._1), keysWeights.map(_._2).opt, aggregation.toOpt))

  /** Executes [[http://redis.io/commands/bzpopmax BZPOPMAX]] */
  def bzpopmax(key: Key, timeout: Int): Result[Opt[(Value, Double)]] =
    execute(new Bzpopmax(new SingletonSeq(key), timeout).map(_.map { case (_, v, s) => (v, s) }))
  /** Executes [[http://redis.io/commands/bzpopmax BZPOPMAX]] */
  def bzpopmax(keys: Iterable[Key], timeout: Int): Result[Opt[(Key, Value, Double)]] =
    execute(new Bzpopmax(keys, timeout))
  /** Executes [[http://redis.io/commands/bzpopmin BZPOPMIN]] */
  def bzpopmin(key: Key, timeout: Int): Result[Opt[(Value, Double)]] =
    execute(new Bzpopmin(new SingletonSeq(key), timeout).map(_.map { case (_, v, s) => (v, s) }))
  /** Executes [[http://redis.io/commands/bzpopmin BZPOPMIN]] */
  def bzpopmin(keys: Iterable[Key], timeout: Int): Result[Opt[(Key, Value, Double)]] =
    execute(new Bzpopmin(keys, timeout))

  private abstract class AbstractZadd[T](decoder: ReplyDecoder[T])
    (key: Key, memberScores: TraversableOnce[(Value, Double)], existence: Opt[Boolean], changed: Boolean, incr: Boolean)
    extends AbstractRedisCommand[T](decoder) with NodeCommand {

    val encoded: Encoded = encoder("ZADD").key(key).optAdd(existence.map(e => if (e) "XX" else "NX"))
      .addFlag("CH", changed).addFlag("INCR", incr).argDataPairs(memberScores.map(_.swap)).result
  }

  private final class Zadd(key: Key, memberScores: TraversableOnce[(Value, Double)], emptyData: Boolean, existence: Opt[Boolean], changed: Boolean)
    extends AbstractZadd[Int](integerInt)(key, memberScores, existence, changed, incr = false) {
    override def immediateResult: Opt[Int] = if (emptyData) Opt(0) else Opt.Empty
  }

  private final class ZaddIncr(key: Key, member: Value, score: Double, existence: Opt[Boolean])
    extends AbstractZadd[Opt[Double]](nullBulkOr(bulkDouble))(key, (member, score).single, existence, changed = false, incr = true)

  private final class Zcard(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZCARD").key(key).result
  }

  private final class Zcount(key: Key, min: ScoreLimit, max: ScoreLimit) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZCOUNT").key(key).add(min.repr).add(max.repr).result
  }

  private final class Zincrby(key: Key, increment: Double, member: Value) extends RedisDoubleCommand with NodeCommand {
    val encoded: Encoded = encoder("ZINCRBY").key(key).add(increment).data(member).result
  }

  private final class Zinterstore(destination: Key, keys: Iterable[Key], weights: Opt[Iterable[Double]], aggregation: Opt[Aggregation])
    extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZINTERSTORE").key(destination).add(keys.size).keys(keys)
      .optAdd("WEIGHTS", weights).optAdd("AGGREGATE", aggregation).result
  }

  private final class Zlexcount(key: Key, min: LexLimit[Value], max: LexLimit[Value])
    extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZLEXCOUNT").key(key).add(LexLimit.repr(min)).add(LexLimit.repr(max)).result
  }

  private abstract class AbstractZrange[T](cmd: String, decoder: ReplyDecoder[Seq[T]])(
    key: Key, start: Long, stop: Long, withscores: Boolean
  ) extends AbstractRedisCommand[Seq[T]](decoder) with NodeCommand {
    val encoded: Encoded = encoder(cmd).key(key).add(start).add(stop).addFlag("WITHSCORES", withscores).result
  }

  private final class Zpopmin(key: Key, count: Opt[Long])
    extends AbstractRedisCommand[Seq[(Value, Double)]](flatMultiBulkSeq(bulk[Value], bulkDouble)) with NodeCommand {
    val encoded: Encoded = encoder("ZPOPMIN").key(key).optAdd(count).result
  }

  private final class Zpopmax(key: Key, count: Opt[Long])
    extends AbstractRedisCommand[Seq[(Value, Double)]](flatMultiBulkSeq(bulk[Value], bulkDouble)) with NodeCommand {
    val encoded: Encoded = encoder("ZPOPMAX").key(key).optAdd(count).result
  }

  private final class Zrange(key: Key, start: Long, stop: Long)
    extends AbstractZrange[Value]("ZRANGE", multiBulkSeq[Value])(key, start, stop, withscores = false)

  private final class ZrangeWithscores(key: Key, start: Long, stop: Long)
    extends AbstractZrange[(Value, Double)]("ZRANGE", flatMultiBulkSeq(bulk[Value], bulkDouble))(
      key, start, stop, withscores = true)

  private final class Zrangebylex(key: Key, min: LexLimit[Value], max: LexLimit[Value], limit: Opt[Limit])
    extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded: Encoded =
      encoder("ZRANGEBYLEX").key(key).add(LexLimit.repr(min)).add(LexLimit.repr(max)).optAdd("LIMIT", limit).result
  }

  private abstract class AbstractZrangebyscore[T](cmd: String, decoder: ReplyDecoder[Seq[T]])(
    key: Key, firstLimit: ScoreLimit, secondLimit: ScoreLimit, withscores: Boolean, limit: Opt[Limit])
    extends AbstractRedisCommand[Seq[T]](decoder) with NodeCommand {
    val encoded: Encoded =
      encoder(cmd).key(key).add(firstLimit.repr).add(secondLimit.repr)
        .addFlag("WITHSCORES", withscores).optAdd("LIMIT", limit).result
  }

  private final class Zrangebyscore(key: Key, min: ScoreLimit, max: ScoreLimit, limit: Opt[Limit])
    extends AbstractZrangebyscore[Value]("ZRANGEBYSCORE", multiBulkSeq[Value])(key, min, max, withscores = false, limit)

  private final class ZrangebyscoreWithscores(key: Key, min: ScoreLimit, max: ScoreLimit, limit: Opt[Limit])
    extends AbstractZrangebyscore[(Value, Double)]("ZRANGEBYSCORE", flatMultiBulkSeq(bulk[Value], bulkDouble))(
      key, min, max, withscores = true, limit)

  private final class Zrank(key: Key, member: Value) extends RedisOptLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZRANK").key(key).data(member).result
  }

  private final class Zrem(key: Key, members: Iterable[Value]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("ZREM").key(key).datas(members).result
    override def immediateResult: Opt[Int] = whenEmpty(members, 0)
  }

  private final class Zremrangebylex(key: Key, min: LexLimit[Value], max: LexLimit[Value])
    extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZREMRANGEBYLEX").key(key).add(LexLimit.repr(min)).add(LexLimit.repr(max)).result
  }

  private final class Zremrangebyrank(key: Key, start: Long, stop: Long)
    extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZREMRANGEBYRANK").key(key).add(start).add(stop).result
  }

  private final class Zremrangebyscore(key: Key, min: ScoreLimit, max: ScoreLimit)
    extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZREMRANGEBYSCORE").key(key).add(min.repr).add(max.repr).result
  }

  private final class Zrevrange(key: Key, start: Long, stop: Long)
    extends AbstractZrange[Value]("ZREVRANGE", multiBulkSeq[Value])(key, start, stop, withscores = false)

  private final class ZrevrangeWithscores(key: Key, start: Long, stop: Long)
    extends AbstractZrange[(Value, Double)]("ZREVRANGE", flatMultiBulkSeq(bulk[Value], bulkDouble))(key, start, stop, withscores = true)

  private final class Zrevrangebylex(key: Key, max: LexLimit[Value], min: LexLimit[Value], limit: Opt[Limit])
    extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("ZREVRANGEBYLEX").key(key).add(LexLimit.repr(max)).add(LexLimit.repr(min)).optAdd("LIMIT", limit).result
  }

  private final class Zrevrangebyscore(key: Key, max: ScoreLimit, min: ScoreLimit, limit: Opt[Limit])
    extends AbstractZrangebyscore[Value]("ZREVRANGEBYSCORE", multiBulkSeq[Value])(key, max, min, withscores = false, limit)

  private final class ZrevrangebyscoreWithscores(key: Key, max: ScoreLimit, min: ScoreLimit, limit: Opt[Limit])
    extends AbstractZrangebyscore[(Value, Double)]("ZREVRANGEBYSCORE", flatMultiBulkSeq(bulk[Value], bulkDouble))(
      key, max, min, withscores = true, limit)

  private final class Zrevrank(key: Key, member: Value) extends RedisOptLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZREVRANK").key(key).data(member).result
  }

  private final class Zscan(key: Key, cursor: Cursor, matchPattern: Opt[Value], count: Opt[Int])
    extends RedisScanCommand[(Value, Double)](flatMultiBulkSeq(bulk[Value], bulkDouble)) with NodeCommand {
    val encoded: Encoded = encoder("ZSCAN").key(key).add(cursor.raw).optData("MATCH", matchPattern).optAdd("COUNT", count).result
  }

  private final class Zscore(key: Key, member: Value) extends RedisOptDoubleCommand with NodeCommand {
    val encoded: Encoded = encoder("ZSCORE").key(key).data(member).result
  }

  private final class Zunionstore(destination: Key, keys: Iterable[Key], weights: Opt[Iterable[Double]], aggregation: Opt[Aggregation])
    extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("ZUNIONSTORE").key(destination).add(keys.size).keys(keys)
      .optAdd("WEIGHTS", weights).optAdd("AGGREGATE", aggregation).result
  }

  private final class Bzpopmax(keys: Iterable[Key], timeout: Int)
    extends AbstractRedisCommand[Opt[(Key, Value, Double)]](multiBulkZTriple[Key, Value]) with NodeCommand {
    val encoded: Encoded = encoder("BZPOPMAX").keys(keys).add(timeout).result

    override def immediateResult: Opt[Opt[(Key, Value, Double)]] =
      if (keys.isEmpty) Opt(Opt.Empty) else Opt.Empty
    override def maxBlockingMillis: Int =
      if (timeout <= 0) Int.MaxValue else timeout * 1000
  }

  private final class Bzpopmin(keys: Iterable[Key], timeout: Int)
    extends AbstractRedisCommand[Opt[(Key, Value, Double)]](multiBulkZTriple[Key, Value]) with NodeCommand {
    val encoded: Encoded = encoder("BZPOPMIN").keys(keys).add(timeout).result

    override def immediateResult: Opt[Opt[(Key, Value, Double)]] =
      if (keys.isEmpty) Opt(Opt.Empty) else Opt.Empty
    override def maxBlockingMillis: Int =
      if (timeout <= 0) Int.MaxValue else timeout * 1000
  }
}

case class ScoreLimit(value: Double, inclusive: Boolean) {
  def repr: String = (if (!inclusive) "(" else "") + (value match {
    case Double.NegativeInfinity => "-inf"
    case Double.PositiveInfinity => "+inf"
    case _ => value.toString
  })
}
object ScoreLimit {
  def incl(value: Double) = ScoreLimit(value, inclusive = true)
  def excl(value: Double) = ScoreLimit(value, inclusive = false)

  val MinusInf: ScoreLimit = ScoreLimit.incl(Double.NegativeInfinity)
  val PlusInf: ScoreLimit = ScoreLimit.incl(Double.PositiveInfinity)
}

sealed trait LexLimit[+V]
object LexLimit {
  def apply[V](value: V, inclusive: Boolean): LexLimit[V] = Finite(value, inclusive)

  def incl[V](value: V): LexLimit[V] = Finite(value, inclusive = true)
  def excl[V](value: V): LexLimit[V] = Finite(value, inclusive = false)

  case class Finite[+V](value: V, inclusive: Boolean) extends LexLimit[V]
  object MinusInf extends LexLimit[Nothing]
  object PlusInf extends LexLimit[Nothing]

  def repr[V: RedisDataCodec](limit: LexLimit[V]): ByteString = limit match {
    case Finite(value, incl) =>
      (if (incl) '[' else '(').toByte +: RedisDataCodec.write(value)
    case MinusInf => ByteString('-'.toByte)
    case PlusInf => ByteString('+'.toByte)
  }
}

case class Limit(offset: Long, count: Long)
object Limit {
  implicit val commandArg: CommandArg[Limit] =
    CommandArg((e, l) => e.add(l.offset).add(l.count))
}

sealed abstract class Aggregation(val name: String) extends NamedEnum
object Aggregation extends NamedEnumCompanion[Aggregation] {
  case object Sum extends Aggregation("SUM")
  case object Min extends Aggregation("MIN")
  case object Max extends Aggregation("MAX")
  val values: List[Aggregation] = caseObjects
}
