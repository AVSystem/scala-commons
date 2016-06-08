package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.exception.UnexpectedReplyException
import com.avsystem.commons.redis.protocol.{ArrayMsg, BulkStringMsg, IntegerMsg, NullBulkStringMsg, SimpleStringStr}
import com.avsystem.commons.redis.{NodeAddress, RedisBinarySeqCommand, RedisBooleanCommand, RedisCommand, RedisLongCommand, RedisOptBinaryCommand, RedisOptLongCommand, RedisUnitCommand, Scope, SimpleSingleKeyed}

import scala.collection.mutable.ArrayBuffer

/**
  * Author: ghik
  * Created: 06/04/16.
  */
trait KeysApi[+F[_]] extends ClusterApiSubset[F] {
  type Self[G[_]] <: KeysApi[G]

  def del(keys: Seq[ByteString]) =
    mapper(Del(keys))
  def dump(key: ByteString) =
    mapper(Dump(key))
  def exists(keys: Seq[ByteString]) =
    mapper(Exists(keys))
  def expire(key: ByteString, seconds: Long) =
    mapper(Expire(key, seconds))
  def expireat(key: ByteString, timestamp: Long) =
    mapper(Expireat(key, timestamp))
  def migrate(keys: Seq[ByteString], address: NodeAddress, destinationDb: Int,
    timeout: Long, copy: Boolean = false, replace: Boolean = false) =
    mapper(Migrate(keys, address, destinationDb, timeout, copy, replace))
  def move(key: ByteString, db: Int) =
    mapper(Move(key, db))

  def objectRefcount(key: ByteString) =
    mapper(ObjectRefcount(key))
  def objectEncoding(key: ByteString) =
    mapper(ObjectEncoding(key))
  def objectIdletime(key: ByteString) =
    mapper(ObjectIdletime(key))

  def persist(key: ByteString): F[Boolean] =
    mapper(Persist(key))
  def pexpire(key: ByteString, milliseconds: Long): F[Boolean] =
    mapper(Pexpire(key, milliseconds))
  def pexpireat(key: ByteString, millisecondsTimestamp: Long): F[Boolean] =
    mapper(Pexpireat(key, millisecondsTimestamp))
  def pttl(key: ByteString): F[Opt[Opt[Long]]] =
    mapper(Pttl(key))

  def rename(key: ByteString, newkey: ByteString): F[Unit] =
    mapper(Rename(key, newkey))
  def renamenx(key: ByteString, newkey: ByteString): F[Boolean] =
    mapper(Renamenx(key, newkey))
  def restore(key: ByteString, ttl: Long, serializedValue: ByteString, replace: Boolean = false): F[Unit] =
    mapper(Restore(key, ttl, serializedValue, replace))

  def sort(key: ByteString, by: Opt[SortPattern] = Opt.Empty, limit: Opt[SortLimit] = Opt.Empty,
    asc: Boolean = true, alpha: Boolean = false): F[Seq[ByteString]] =
    mapper(Sort(key, by, limit, asc, alpha))
  def sortGet(key: ByteString, gets: Seq[SortPattern], by: Opt[SortPattern] = Opt.Empty, limit: Opt[SortLimit] = Opt.Empty,
    asc: Boolean = true, alpha: Boolean = false): F[Seq[Seq[Opt[ByteString]]]] =
    mapper(SortGet(key, gets, by, limit, asc, alpha))
  def sortStore(key: ByteString, destination: ByteString, by: Opt[SortPattern] = Opt.Empty, limit: Opt[SortLimit] = Opt.Empty,
    gets: Seq[SortPattern] = Nil, asc: Boolean = true, alpha: Boolean = false): F[Long] =
    mapper(SortStore(key, destination, by, limit, gets, asc, alpha))

  def ttl(key: ByteString): F[Opt[Opt[Long]]] =
    mapper(Ttl(key))
  def `type`(key: ByteString): F[RedisType] =
    mapper(Type(key))
}

trait NodeKeysApi[+F[_]] extends KeysApi[F] with NodeApiSubset[F] {
  def keys(pattern: ByteString) =
    mapper(Keys(pattern))
  def scan(cursor: Cursor, matchPattern: Opt[ByteString] = Opt.Empty, count: Opt[Long] = Opt.Empty): F[(Cursor, Seq[ByteString])] =
    mapper(Scan(cursor, matchPattern, count))
  def randomkey: F[Opt[ByteString]] =
    mapper(Randomkey)
  def wait(numslaves: Int, timeout: Long): F[Long] =
    mapper(Wait(numslaves, timeout))
}

case class Del(keys: Seq[ByteString]) extends RedisLongCommand[Scope.Cluster] {
  require(keys.nonEmpty, "DEL requires at least one key")
  def encode = encoder("DEL").add(keys).result
  def isKey(idx: Int) = idx > 0
}

case class Dump(key: ByteString) extends RedisOptBinaryCommand[Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("DUMP").add(key).result
}

case class Exists(keys: Seq[ByteString]) extends RedisLongCommand[Scope.Cluster] {
  require(keys.nonEmpty, "EXISTS requires at least one key")
  def encode = encoder("EXISTS").add(keys).result
  def isKey(idx: Int) = idx > 0
}

case class Expire(key: ByteString, seconds: Long) extends RedisBooleanCommand[Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("EXPIRE").add(key).add(seconds).result
}

case class Expireat(key: ByteString, timestamp: Long) extends RedisBooleanCommand[Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("EXPIREAT").add(key).add(timestamp).result
}

case class Keys(pattern: ByteString) extends RedisBinarySeqCommand[Scope.Node] {
  def encode = encoder("KEYS").add(pattern).result
  def isKey(idx: Int) = false
}

case class Migrate(keys: Seq[ByteString], address: NodeAddress, destinationDb: Int,
  timeout: Long, copy: Boolean, replace: Boolean) extends RedisCommand[Boolean, Scope.Cluster] {
  require(keys.nonEmpty, "MIGRATE requires at least one key")

  private val multiKey = keys.size > 1

  def encode = {
    val enc = encoder("MIGRATE").add(address.ip).add(address.port)
    if (multiKey) {
      enc.add("")
    } else {
      enc.add(keys.head)
    }
    enc.add(destinationDb).add(timeout).addFlag("COPY", copy).addFlag("REPLACE", replace)
    if (multiKey) {
      enc.add("KEYS").add(keys)
    }
    enc.result
  }

  def isKey(idx: Int) =
    if (multiKey) {
      var keysOffset = 6
      if (copy) keysOffset += 1
      if (replace) keysOffset += 1
      idx > keysOffset
    } else idx == 3

  def decodeExpected = {
    case SimpleStringStr("OK") => true
    case SimpleStringStr("NOKEY") => false
  }
}

case class Move(key: ByteString, db: Int) extends RedisBooleanCommand[Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("MOVE").add(key).add(db).result
}

case class ObjectRefcount(key: ByteString) extends RedisOptLongCommand[Scope.Cluster] {
  def encode = encoder("OBJECT", "REFCOUNT").add(key).result
  def isKey(idx: Int) = idx == 2
}

case class ObjectEncoding(key: ByteString) extends RedisCommand[Opt[Encoding], Scope.Cluster] {
  def encode = encoder("OBJECT", "ENCODING").add(key).result
  def isKey(idx: Int) = idx == 2
  def decodeExpected = {
    case BulkStringMsg(string) => Opt(Encoding.byName(string.utf8String))
    case NullBulkStringMsg => Opt.Empty
  }
}

case class ObjectIdletime(key: ByteString) extends RedisOptLongCommand[Scope.Cluster] {
  def encode = encoder("OBJECT", "IDLETIME").add(key).result
  def isKey(idx: Int) = idx == 2
}

case class Persist(key: ByteString) extends RedisBooleanCommand[Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("PERSIST").add(key).result
}

case class Pexpire(key: ByteString, milliseconds: Long) extends RedisBooleanCommand[Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("PEXPIRE").add(key).add(milliseconds).result
}

case class Pexpireat(key: ByteString, millisecondsTimestamp: Long) extends RedisBooleanCommand[Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("PEXPIREAT").add(key).add(millisecondsTimestamp).result
}

case class Pttl(key: ByteString) extends RedisCommand[Opt[Opt[Long]], Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("PTTL").add(key).result
  def decodeExpected = {
    case IntegerMsg(-2) => Opt.Empty
    case IntegerMsg(-1) => Opt(Opt.Empty)
    case IntegerMsg(ttl) => Opt(Opt(ttl))
  }
}

case object Randomkey extends RedisOptBinaryCommand[Scope.Cluster] {
  def encode = encoder("RANDOMKEY").result
  def isKey(idx: Int) = false
}

case class Rename(key: ByteString, newkey: ByteString) extends RedisUnitCommand[Scope.Cluster] {
  def encode = encoder("RENAME").add(key).add(newkey).result
  def isKey(idx: Int) = idx == 1 || idx == 2
}

case class Renamenx(key: ByteString, newkey: ByteString) extends RedisBooleanCommand[Scope.Cluster] {
  def encode = encoder("RENAMENX").add(key).add(newkey).result
  def isKey(idx: Int) = idx == 1 || idx == 2
}

case class Restore(key: ByteString, ttl: Long, serializedValue: ByteString, replace: Boolean) extends RedisUnitCommand[Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("RESTORE").add(key).add(ttl).add(serializedValue).addFlag("REPLACE", replace).result
}

case class Scan(cursor: Cursor, matchPattern: Opt[ByteString], count: Opt[Long]) extends RedisCommand[(Cursor, Seq[ByteString]), Scope.Cluster] {
  def encode = encoder("SCAN").add(cursor.raw).optAdd("MATCH", matchPattern).optAdd("COUNT", count).result
  def isKey(idx: Int) = false
  def decodeExpected = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(cursorString), ArrayMsg(elements))) =>
      (Cursor(cursorString.utf8String.toLong), elements.map {
        case BulkStringMsg(bs) => bs
        case msg => throw new UnexpectedReplyException(s"Expected multi bulk reply, but one of the elements is $msg")
      })
  }
}

sealed abstract case class AbstractSort[T](key: ByteString, by: Opt[SortPattern], limit: Opt[SortLimit],
  gets: Seq[SortPattern], asc: Boolean, alpha: Boolean, destination: Opt[ByteString]) extends RedisCommand[T, Scope.Cluster] {
  def encode = {
    val enc = encoder("SORT").add(key).optAdd("BY", by).optAdd("LIMIT", limit)
    gets.foreach(sp => enc.add("GET").add(sp))
    enc.addFlag("DESC", !asc).addFlag("ALPHA", alpha).optAdd("STORE", destination).result
  }
  private val cmdSize = 3 + gets.size * 2 + by.size * 2 + limit.size * 3 + (if (asc) 0 else 1) + (if (alpha) 1 else 0)
  def isKey(idx: Int) = idx == 1 || destination.nonEmpty && idx == cmdSize - 1
}

class Sort(key: ByteString, by: Opt[SortPattern], limit: Opt[SortLimit], asc: Boolean, alpha: Boolean)
  extends AbstractSort[Seq[ByteString]](key, by, limit, Nil, asc, alpha, Opt.Empty) with RedisBinarySeqCommand[Scope.Cluster]
object Sort {
  def apply(key: ByteString, by: Opt[SortPattern] = Opt.Empty, limit: Opt[SortLimit] = Opt.Empty, asc: Boolean, alpha: Boolean) =
    new Sort(key, by, limit, asc, alpha)
}

class SortGet(key: ByteString, gets: Seq[SortPattern], by: Opt[SortPattern], limit: Opt[SortLimit], asc: Boolean, alpha: Boolean)
  extends AbstractSort[Seq[Seq[Opt[ByteString]]]](key, by, limit, gets, asc, alpha, Opt.Empty) {

  def decodeExpected = {
    case ArrayMsg(elements) =>
      val valuesPerKey = gets.size min 1
      val it = elements.iterator.map {
        case NullBulkStringMsg => Opt.Empty
        case BulkStringMsg(bytes) => Opt(bytes)
        case msg => throw new UnexpectedReplyException(s"Expected multi bulk reply but one of the elements is $msg")
      }.grouped(valuesPerKey)
      it.to[ArrayBuffer]
  }
}
object SortGet {
  def apply(key: ByteString, gets: Seq[SortPattern], by: Opt[SortPattern], limit: Opt[SortLimit], asc: Boolean, alpha: Boolean) =
    new SortGet(key, gets, by, limit, asc, alpha)
}

class SortStore(key: ByteString, destination: ByteString, by: Opt[SortPattern], limit: Opt[SortLimit], gets: Seq[SortPattern], asc: Boolean, alpha: Boolean)
  extends AbstractSort[Long](key, by, limit, gets, asc, alpha, Opt(destination)) with RedisLongCommand[Scope.Cluster]

object SortStore {
  def apply(key: ByteString, destination: ByteString, by: Opt[SortPattern], limit: Opt[SortLimit], gets: Seq[SortPattern], asc: Boolean, alpha: Boolean) =
    new SortStore(key, destination, by, limit, gets, asc, alpha)
}

case class Ttl(key: ByteString) extends RedisCommand[Opt[Opt[Long]], Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("TTL").add(key).result
  def decodeExpected = {
    case IntegerMsg(-2) => Opt.Empty
    case IntegerMsg(-1) => Opt(Opt.Empty)
    case IntegerMsg(ttl) => Opt(Opt(ttl))
  }
}

case class Type(key: ByteString) extends RedisCommand[RedisType, Scope.Cluster] with SimpleSingleKeyed {
  def encode = encoder("TYPE").add(key).result
  def decodeExpected = {
    case SimpleStringStr(str) => RedisType.byName(str)
  }
}

case class Wait(numslaves: Int, timeout: Long) extends RedisLongCommand[Scope.Node] {
  def encode = encoder("WAIT").add(numslaves).add(timeout).result
  def isKey(idx: Int) = false
}

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

case class Cursor(raw: Long) extends AnyVal {
  override def toString = raw.toString
}
object Cursor {
  val NoCursor = Cursor(0)
}

case class SortLimit(offset: Long, count: Long)
object SortLimit {
  implicit val SortLimitArg: CommandArg[SortLimit] =
    CommandArg((ce, sl) => ce.add(sl.offset).add(sl.count))
}

sealed abstract class SortPattern(val repr: ByteString)
case object SelfPattern extends SortPattern(ByteString("#"))
case class KeyPattern(pattern: ByteString) extends SortPattern(pattern)
case class HashFieldPattern(keyPattern: ByteString, fieldPattern: ByteString)
  extends SortPattern(keyPattern ++ ByteString("->") ++ fieldPattern)
object SortPattern {
  implicit val SortPatternArg: CommandArg[SortPattern] = CommandArg((ce, sp) => ce.add(sp.repr))
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
