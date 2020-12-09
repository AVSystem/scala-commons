package com.avsystem.commons
package redis.commands

import akka.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.protocol._

/**
  * Author: ghik
  * Created: 06/04/16.
  */
trait KeyedKeysApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/del DEL]] */
  def del(key: Key): Result[Boolean] =
    execute(new Del(key.single).map(_ > 0))

  /** Executes [[http://redis.io/commands/del DEL]] */
  def del(key: Key, keys: Key*): Result[Int] =
    execute(new Del(key +:: keys))

  /** Executes [[http://redis.io/commands/del DEL]]
    * or simply returns 0 if `keys` is empty, without sending the command to Redis */
  def del(keys: Iterable[Key]): Result[Int] =
    execute(new Del(keys))

  /** Executes [[http://redis.io/commands/dump DUMP]] */
  def dump(key: Key): Result[Opt[Dumped]] =
    execute(new Dump(key))

  /** Executes [[http://redis.io/commands/exists EXISTS]] */
  def exists(key: Key): Result[Boolean] =
    execute(new Exists(key.single).map(_ > 0))

  /** Executes [[http://redis.io/commands/exists EXISTS]] */
  def exists(key: Key, keys: Key*): Result[Int] =
    execute(new Exists(key +:: keys))

  /** Executes [[http://redis.io/commands/exists EXISTS]]
    * or simply returns 0 when `keys` is empty, without sending the command to Redis */
  def exists(keys: Iterable[Key]): Result[Int] =
    execute(new Exists(keys))

  /** Executes [[http://redis.io/commands/expire EXPIRE]] */
  def expire(key: Key, seconds: Long): Result[Boolean] =
    execute(new Expire(key, seconds))

  /** Executes [[http://redis.io/commands/expireat EXPIREAT]] */
  def expireat(key: Key, timestamp: Long): Result[Boolean] =
    execute(new Expireat(key, timestamp))

  /** Executes [[http://redis.io/commands/migrate MIGRATE]]
    * or simply returns `true` when `keys` is empty, without sending the command to Redis */
  def migrate(keys: Iterable[Key], address: NodeAddress, destinationDb: Int,
    timeout: Long, copy: Boolean = false, replace: Boolean = false): Result[Boolean] =
    execute(new Migrate(keys, address, destinationDb, timeout, copy, replace))

  /** Executes [[http://redis.io/commands/object OBJECT]] */
  def objectRefcount(key: Key): Result[Opt[Long]] =
    execute(new ObjectRefcount(key))

  /** Executes [[http://redis.io/commands/object OBJECT]] */
  def objectEncoding(key: Key): Result[Opt[Encoding]] =
    execute(new ObjectEncoding(key))

  /** Executes [[http://redis.io/commands/object OBJECT]] */
  def objectIdletime(key: Key): Result[Opt[Long]] =
    execute(new ObjectIdletime(key))

  def memoryUsage(key: Key, samples: OptArg[Long] = OptArg.Empty): Result[Opt[Long]] =
    execute(new MemoryUsage(key, samples.toOpt))

  /** Executes [[http://redis.io/commands/persist PERSIST]] */
  def persist(key: Key): Result[Boolean] =
    execute(new Persist(key))

  /** Executes [[http://redis.io/commands/pexpire PEXPIRE]] */
  def pexpire(key: Key, milliseconds: Long): Result[Boolean] =
    execute(new Pexpire(key, milliseconds))

  /** Executes [[http://redis.io/commands/pexpireat PEXPIREAT]] */
  def pexpireat(key: Key, millisecondsTimestamp: Long): Result[Boolean] =
    execute(new Pexpireat(key, millisecondsTimestamp))

  /** Executes [[http://redis.io/commands/pttl PTTL]] */
  def pttl(key: Key): Result[Opt[Opt[Long]]] =
    execute(new Pttl(key))

  /** Executes [[http://redis.io/commands/rename RENAME]] */
  def rename(key: Key, newkey: Key): Result[Unit] =
    execute(new Rename(key, newkey))

  /** Executes [[http://redis.io/commands/renamenx RENAMENX]] */
  def renamenx(key: Key, newkey: Key): Result[Boolean] =
    execute(new Renamenx(key, newkey))

  /** Executes [[http://redis.io/commands/restore RESTORE]] */
  def restore(key: Key, ttl: Long, dumpedValue: Dumped, replace: Boolean = false): Result[Unit] =
    execute(new Restore(key, ttl, dumpedValue, replace))

  /** Executes [[http://redis.io/commands/sort SORT]] */
  def sort(key: Key, by: OptArg[SortPattern[Key, Field]] = OptArg.Empty, limit: OptArg[SortLimit] = OptArg.Empty,
    sortOrder: OptArg[SortOrder] = OptArg.Empty, alpha: Boolean = false): Result[Seq[Value]] =
    execute(new Sort(key, by.toOpt, limit.toOpt, sortOrder.toOpt, alpha))

  /** Executes [[http://redis.io/commands/sort SORT]] */
  def sortGet(key: Key, gets: Seq[SortPattern[Key, Field]], by: OptArg[SortPattern[Key, Field]] = OptArg.Empty, limit: OptArg[SortLimit] = OptArg.Empty,
    sortOrder: OptArg[SortOrder] = OptArg.Empty, alpha: Boolean = false): Result[Seq[Seq[Opt[Value]]]] =
    execute(new SortGet(key, gets, by.toOpt, limit.toOpt, sortOrder.toOpt, alpha))

  /** Executes [[http://redis.io/commands/sort SORT]] */
  def sortStore(key: Key, destination: Key, by: OptArg[SortPattern[Key, Field]] = OptArg.Empty, limit: OptArg[SortLimit] = OptArg.Empty,
    gets: Seq[SortPattern[Key, Field]] = Nil, sortOrder: OptArg[SortOrder] = OptArg.Empty, alpha: Boolean = false): Result[Long] =
    execute(new SortStore(key, destination, by.toOpt, limit.toOpt, gets, sortOrder.toOpt, alpha))

  /** Executes [[http://redis.io/commands/touch TOUCH]] */
  def touch(key: Key): Result[Boolean] =
    execute(new Touch(key.single).map(_ > 0))

  /** Executes [[http://redis.io/commands/touch TOUCH]] */
  def touch(key: Key, keys: Key*): Result[Int] =
    execute(new Touch(key +:: keys))

  /** Executes [[http://redis.io/commands/touch TOUCH]]
    * or simply returns 0 when `keys` is empty, without sending the command to Redis */
  def touch(keys: Iterable[Key]): Result[Int] =
    execute(new Touch(keys))

  /** Executes [[http://redis.io/commands/ttl TTL]] */
  def ttl(key: Key): Result[Opt[Opt[Long]]] =
    execute(new Ttl(key))

  /** Executes [[http://redis.io/commands/type TYPE]] */
  def `type`(key: Key): Result[RedisType] =
    execute(new Type(key))

  /** Executes [[http://redis.io/commands/unlink UNLINK]] */
  def unlink(key: Key): Result[Boolean] =
    execute(new Unlink(key.single).map(_ > 0))

  /** Executes [[http://redis.io/commands/unlink UNLINK]] */
  def unlink(key: Key, keys: Key*): Result[Int] =
    execute(new Unlink(key +:: keys))

  /** Executes [[http://redis.io/commands/unlink UNLINK]]
    * or simply returns 0 when `keys` is empty, without sending the command to Redis */
  def unlink(keys: Iterable[Key]): Result[Int] =
    execute(new Unlink(keys))

  private final class Del(keys: Iterable[Key]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("DEL").keys(keys).result
    override def immediateResult: Opt[Int] = whenEmpty(keys, 0)
  }

  private final class Dump(key: Key) extends RedisOptCommand[Dumped](bulk(Dumped)) with NodeCommand {
    val encoded: Encoded = encoder("DUMP").key(key).result
  }

  private final class Exists(keys: Iterable[Key]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("EXISTS").keys(keys).result
    override def immediateResult: Opt[Int] = whenEmpty(keys, 0)
  }

  private final class Expire(key: Key, seconds: Long) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("EXPIRE").key(key).add(seconds).result
  }

  private final class Expireat(key: Key, timestamp: Long) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("EXPIREAT").key(key).add(timestamp).result
  }

  private final class Migrate(keys: Iterable[Key], address: NodeAddress, destinationDb: Int,
    timeout: Long, copy: Boolean, replace: Boolean) extends RedisCommand[Boolean] with NodeCommand {

    private val multiKey = keys.size != 1

    val encoded: Encoded = {
      val enc = encoder("MIGRATE").add(address.ip).add(address.port)
      if (multiKey) {
        enc.add(ByteString.empty)
      } else {
        enc.key(keys.head)
      }
      enc.add(destinationDb).add(timeout).addFlag("COPY", copy).addFlag("REPLACE", replace)
      if (multiKey) {
        enc.add("KEYS").keys(keys)
      }
      enc.result
    }

    def decodeExpected: ReplyDecoder[Boolean] = {
      case RedisMsg.Ok => true
      case RedisMsg.Nokey => false
    }

    override def immediateResult: Opt[Boolean] = whenEmpty(keys, true)
  }

  private final class ObjectRefcount(key: Key) extends RedisOptLongCommand with NodeCommand {
    val encoded: Encoded = encoder("OBJECT", "REFCOUNT").key(key).result
  }

  private final class ObjectEncoding(key: Key)
    extends RedisOptCommand[Encoding](bulkAsNamedEnum(Encoding)) with NodeCommand {
    val encoded: Encoded = encoder("OBJECT", "ENCODING").key(key).result
  }

  private final class ObjectIdletime(key: Key) extends RedisOptLongCommand with NodeCommand {
    val encoded: Encoded = encoder("OBJECT", "IDLETIME").key(key).result
  }

  private final class MemoryUsage(key: Key, samples: Opt[Long]) extends RedisOptLongCommand with NodeCommand {
    val encoded: Encoded = encoder("MEMORY", "USAGE").key(key).optAdd("SAMPLES", samples).result
  }

  private final class Persist(key: Key) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("PERSIST").key(key).result
  }

  private final class Pexpire(key: Key, milliseconds: Long) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("PEXPIRE").key(key).add(milliseconds).result
  }

  private final class Pexpireat(key: Key, millisecondsTimestamp: Long) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("PEXPIREAT").key(key).add(millisecondsTimestamp).result
  }

  private final class Pttl(key: Key) extends AbstractRedisCommand[Opt[Opt[Long]]](integerAsTtl) with NodeCommand {
    val encoded: Encoded = encoder("PTTL").key(key).result
  }

  private final class Rename(key: Key, newkey: Key) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("RENAME").key(key).key(newkey).result
  }

  private final class Renamenx(key: Key, newkey: Key) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("RENAMENX").key(key).key(newkey).result
  }

  private final class Restore(key: Key, ttl: Long, dumpedValue: Dumped, replace: Boolean)
    extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("RESTORE").key(key).add(ttl).add(dumpedValue.raw).addFlag("REPLACE", replace).result
  }

  private abstract class AbstractSort[T](decoder: ReplyDecoder[T])
    (key: Key, by: Opt[SortPattern[Key, Field]], limit: Opt[SortLimit],
      gets: Seq[SortPattern[Key, Field]], sortOrder: Opt[SortOrder], alpha: Boolean, destination: Opt[Key])
    extends AbstractRedisCommand[T](decoder) with NodeCommand {
    val encoded: Encoded = {
      val enc = encoder("SORT").key(key).optAdd("BY", by).optAdd("LIMIT", limit)
      gets.foreach(sp => enc.add("GET").add(sp))
      enc.optAdd(sortOrder).addFlag("ALPHA", alpha).optKey("STORE", destination).result
    }
  }

  private final class Sort(key: Key, by: Opt[SortPattern[Key, Field]], limit: Opt[SortLimit], sortOrder: Opt[SortOrder], alpha: Boolean)
    extends AbstractSort[Seq[Value]](multiBulkAsSeqOf[Value])(key, by, limit, Nil, sortOrder, alpha, Opt.Empty)

  private final class SortGet(key: Key, gets: Seq[SortPattern[Key, Field]], by: Opt[SortPattern[Key, Field]], limit: Opt[SortLimit], sortOrder: Opt[SortOrder], alpha: Boolean)
    extends AbstractSort[Seq[Seq[Opt[Value]]]](multiBulkAsGroupedSeq(gets.size min 1, nullBulkOrAs[Value]))(
      key, by, limit, gets, sortOrder, alpha, Opt.Empty)

  private final class SortStore(key: Key, destination: Key, by: Opt[SortPattern[Key, Field]], limit: Opt[SortLimit], gets: Seq[SortPattern[Key, Field]], sortOrder: Opt[SortOrder], alpha: Boolean)
    extends AbstractSort[Long](integerAsLong)(key, by, limit, gets, sortOrder, alpha, Opt(destination))

  private final class Touch(keys: Iterable[Key]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("TOUCH").keys(keys).result
    override def immediateResult: Opt[Int] = whenEmpty(keys, 0)
  }

  private final class Ttl(key: Key) extends AbstractRedisCommand[Opt[Opt[Long]]](integerAsTtl) with NodeCommand {
    val encoded: Encoded = encoder("TTL").key(key).result
  }

  private final class Type(key: Key) extends AbstractRedisCommand[RedisType](simpleAs[RedisType]) with NodeCommand {
    val encoded: Encoded = encoder("TYPE").key(key).result
  }

  private final class Unlink(keys: Iterable[Key]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("UNLINK").keys(keys).result
    override def immediateResult: Opt[Int] = whenEmpty(keys, 0)
  }
}

trait NodeKeysApi extends KeyedKeysApi with ApiSubset {
  /** Executes [[http://redis.io/commands/move MOVE]] */
  def move(key: Key, db: Int): Result[Boolean] =
    execute(new Move(key, db))
  /** Executes [[http://redis.io/commands/keys KEYS]] */
  def keys(pattern: Key): Result[BSet[Key]] =
    execute(new Keys(pattern))
  /** Executes [[http://redis.io/commands/scan SCAN]] */
  def scan(cursor: Cursor, matchPattern: OptArg[Key] = OptArg.Empty, count: OptArg[Long] = OptArg.Empty): Result[(Cursor, Seq[Key])] =
    execute(new Scan(cursor, matchPattern.toOpt, count.toOpt))
  /** Executes [[http://redis.io/commands/randomkey RANDOMKEY]] */
  def randomkey: Result[Opt[Key]] =
    execute(Randomkey)
  /** Executes [[http://redis.io/commands/wait WAIT]] */
  def wait(numslaves: Int, timeout: Long): Result[Long] =
    execute(new Wait(numslaves, timeout))

  private final class Move(key: Key, db: Int) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("MOVE").key(key).add(db).result
  }

  private final class Keys(pattern: Key) extends RedisDataSetCommand[Key] with NodeCommand {
    val encoded: Encoded = encoder("KEYS").data(pattern).result
  }

  private final class Scan(cursor: Cursor, matchPattern: Opt[Key], count: Opt[Long])
    extends RedisScanCommand[Key](multiBulkAsSeqOf[Key]) with NodeCommand {
    val encoded: Encoded = encoder("SCAN").add(cursor.raw).optData("MATCH", matchPattern).optAdd("COUNT", count).result
  }

  private object Randomkey extends RedisOptDataCommand[Key] with NodeCommand {
    val encoded: Encoded = encoder("RANDOMKEY").result
  }

  private final class Wait(numslaves: Int, timeout: Long) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("WAIT").add(numslaves).add(timeout).result
  }
}

case class Dumped(raw: ByteString) extends AnyVal

sealed abstract class Encoding(val name: String) extends NamedEnum
sealed trait StringEncoding extends Encoding
sealed trait ListEncoding extends Encoding
sealed trait SetEncoding extends Encoding
sealed trait HashEncoding extends Encoding
sealed trait SortedSetEncoding extends Encoding

object Encoding extends NamedEnumCompanion[Encoding] {
  case object Raw extends Encoding("raw") with StringEncoding
  case object Int extends Encoding("int") with StringEncoding
  case object ZipList extends Encoding("ziplist") with ListEncoding with HashEncoding with SortedSetEncoding
  case object LinkedList extends Encoding("linkedlist") with ListEncoding
  case object IntSet extends Encoding("intset") with SetEncoding
  case object HashTable extends Encoding("hashtable") with SetEncoding with HashEncoding
  case object SkipList extends Encoding("skiplist") with SortedSetEncoding
  case object EmbStr extends Encoding("embstr") with StringEncoding

  val values: List[Encoding] = caseObjects
}

case class SortLimit(offset: Long, count: Long)
object SortLimit {
  implicit val SortLimitArg: CommandArg[SortLimit] =
    CommandArg((ce, sl) => ce.add(sl.offset).add(sl.count))
}

sealed trait SortPattern[+K, +F]
case object SelfPattern extends SortPattern[Nothing, Nothing]
case class KeyPattern[+K](pattern: K) extends SortPattern[K, Nothing]
case class FieldPattern[+K, +F](keyPattern: K, fieldPattern: F) extends SortPattern[K, F]
object SortPattern {
  implicit def SortPatternArg[K: RedisDataCodec, F: RedisDataCodec]: CommandArg[SortPattern[K, F]] =
    CommandArg((ce, sp) => ce.add(sp match {
      case SelfPattern => ByteString("#")
      case KeyPattern(pattern) => RedisDataCodec.write(pattern)
      case FieldPattern(keyPattern, fieldPattern) =>
        val bsb = new ByteStringBuilder
        bsb.append(RedisDataCodec.write(keyPattern))
        bsb.append(ByteString("->"))
        bsb.append(RedisDataCodec.write(fieldPattern))
        bsb.result()
    }))
}

sealed abstract class RedisType(val name: String) extends NamedEnum
object RedisType extends NamedEnumCompanion[RedisType] {
  case object None extends RedisType("none")
  case object String extends RedisType("string")
  case object List extends RedisType("list")
  case object Set extends RedisType("set")
  case object Zset extends RedisType("zset")
  case object Hash extends RedisType("hash")

  val values: List[RedisType] = caseObjects
}
