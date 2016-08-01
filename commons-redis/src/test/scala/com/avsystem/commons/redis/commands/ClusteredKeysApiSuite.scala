package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.ClusterUtils.keyWithSameSlotAs
import com.avsystem.commons.redis.{CommandsSuite, RedisClusterCommandsSuite, RedisCommands, RedisConnectionCommandsSuite, RedisNodeCommandsSuite}

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait ClusteredKeysApiSuite extends CommandsSuite {
  type Api <: ClusteredKeysApi

  import RedisCommands._

  test("DEL") {
    setup(set(bs"key", bs"value"))
    assert(del(Seq(bs"key")).exec.futureValue == 1)
  }

  test("DUMP") {
    setup(set(bs"key", bs"value"))
    assert(dump(bs"???").exec.futureValue.isEmpty)
    assert(dump(bs"key").exec.futureValue.nonEmpty)
  }

  test("EXISTS") {
    setup(set(bs"key", bs"value"))
    assert(exists(Seq(bs"???")).exec.futureValue == 0)
    assert(exists(Seq(bs"key")).exec.futureValue == 1)
  }

  test("EXPIRE") {
    setup(set(bs"key", bs"value"))
    assert(expire(bs"key", Int.MaxValue).exec.futureValue)
  }

  test("EXPIREAT") {
    setup(set(bs"key", bs"value"))
    assert(expireat(bs"key", Int.MaxValue).exec.futureValue)
  }

  test("OBJECT REFCOUNT") {
    setup(set(bs"key", bs"value"))
    assert(objectRefcount(bs"???").exec.futureValue.isEmpty)
    assert(objectRefcount(bs"key").exec.futureValue.nonEmpty)
  }

  test("OBJECT ENCODING") {
    setup(set(bs"key", bs"value"))
    assert(objectEncoding(bs"???").exec.futureValue.isEmpty)
    assert(objectEncoding(bs"key").exec.futureValue.nonEmpty)
  }

  test("OBJECT IDLETIME") {
    setup(set(bs"key", bs"value"))
    assert(objectIdletime(bs"???").exec.futureValue.isEmpty)
    assert(objectIdletime(bs"key").exec.futureValue.nonEmpty)
  }

  test("PERSIST") {
    setup(set(bs"key", bs"value"))
    assert(!persist(bs"key").exec.futureValue)
  }

  test("PEXPIRE") {
    setup(set(bs"key", bs"value"))
    assert(pexpire(bs"key", Int.MaxValue).exec.futureValue)
  }

  test("PEXPIREAT") {
    setup(set(bs"key", bs"value"))
    assert(pexpireat(bs"key", Int.MaxValue).exec.futureValue)
  }

  test("PTTL") {
    setup(
      set(bs"key", bs"value"),
      setex(bs"exkey", Int.MaxValue, bs"value")
    )
    assert(pttl(bs"???").exec.futureValue == Opt.Empty)
    assert(pttl(bs"key").exec.futureValue == Opt(Opt.Empty))
    assert(pttl(bs"exkey").exec.futureValue.exists(_.nonEmpty))
  }

  test("RENAME") {
    setup(set(bs"key", bs"value"))
    rename(bs"key", keyWithSameSlotAs(bs"key")).exec.futureValue
  }

  test("RENAMENX") {
    setup(set(bs"key", bs"value"))
    assert(renamenx(bs"key", keyWithSameSlotAs(bs"key")).exec.futureValue)
  }

  test("RESTORE") {
    setup(set(bs"key", bs"value"))
    val dumped = dump(bs"key").exec.futureValue.get
    restore(bs"torestore", 1, dumped).exec.futureValue
  }

  test("SORT") {
    assert(sort(bs"somelist",
      Opt(SelfPattern), Opt(SortLimit(0, 1)), asc = false, alpha = true).exec.futureValue.isEmpty)
  }

  test("SORT with STORE") {
    assert(sortStore(bs"somelist", keyWithSameSlotAs(bs"somelist")).exec.futureValue == 0)
  }

  test("TTL") {
    setup(
      set(bs"key", bs"value"),
      setex(bs"exkey", Int.MaxValue, bs"value")
    )
    assert(ttl(bs"???").exec.futureValue == Opt.Empty)
    assert(ttl(bs"key").exec.futureValue == Opt(Opt.Empty))
    assert(ttl(bs"exkey").exec.futureValue.exists(_.nonEmpty))
  }

  test("TYPE") {
    setup(set(bs"key", bs"value"))
    assert(`type`(bs"key").exec.futureValue == RedisType.String)
  }
}

trait NodeKeysApiSuite extends ClusteredKeysApiSuite {
  type Api <: NodeKeysApi

  import RedisCommands._

  private val scanKeys = (0 until 32).map(i => bs"toscan$i")

  test("KEYS") {
    setup(mset(scanKeys.map(k => (k, bs"value"))))
    assert(keys(bs"toscan*").exec.futureValue.toSet == scanKeys.toSet)
  }

  test("SCAN") {
    setup(mset(scanKeys.map(k => (k, bs"value"))))
    def scanCollect(cursor: Cursor, acc: Seq[ByteString]): Future[Seq[ByteString]] =
      scan(cursor, Opt(bs"toscan*"), Opt(4L)).exec.flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data)
        case (nextCursor, data) => scanCollect(nextCursor, acc ++ data)
      }
    assert(scanCollect(Cursor.NoCursor, Vector.empty).futureValue.toSet == scanKeys.toSet)
  }

  test("DEL multikey") {
    setup(set(bs"key", bs"value"))
    assert(del(Seq(bs"key", bs"foo")).exec.futureValue == 1)
  }

  test("MOVE") {
    setup(set(bs"key", bs"value"))
    assert(move(bs"key", 1).exec.futureValue)
  }

  test("EXISTS multikey") {
    setup(set(bs"key", bs"value"))
    assert(exists(Seq(bs"key", bs"foo")).exec.futureValue == 1)
  }

  test("SORT with GET") {
    assert(sortGet(bs"somelist", Seq(HashFieldPattern(bs"hash", bs"*")),
      Opt(SelfPattern), Opt(SortLimit(0, 1)), asc = false, alpha = true).exec.futureValue.isEmpty)
  }

  test("SORT with BY") {
    assert(sort(bs"somelist", by = SelfPattern.opt).exec.futureValue.isEmpty)
    assert(sort(bs"somelist", by = KeyPattern(bs"sth_*").opt).exec.futureValue.isEmpty)
    assert(sort(bs"somelist", by = HashFieldPattern(bs"hash_*", bs"sth_*").opt).exec.futureValue.isEmpty)
  }
}

class RedisClusterKeysApiSuite extends RedisClusterCommandsSuite with ClusteredKeysApiSuite
class RedisNodeKeysApiSuite extends RedisNodeCommandsSuite with NodeKeysApiSuite
class RedisConnectionKeysApiSuite extends RedisConnectionCommandsSuite with NodeKeysApiSuite
