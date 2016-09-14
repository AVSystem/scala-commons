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
  import RedisCommands._

  test("DEL") {
    setup(set(bs"key", bs"value"))
    del(Seq(bs"key")).assertEquals(1)
  }

  test("DUMP") {
    setup(set(bs"key", bs"value"))
    dump(bs"???").assert(_.isEmpty)
    dump(bs"key").assert(_.nonEmpty)
  }

  test("EXISTS") {
    setup(set(bs"key", bs"value"))
    exists(Seq(bs"???")).assertEquals(0)
    exists(Seq(bs"key")).assertEquals(1)
  }

  test("EXPIRE") {
    setup(set(bs"key", bs"value"))
    expire(bs"key", Int.MaxValue).assert(identity)
  }

  test("EXPIREAT") {
    setup(set(bs"key", bs"value"))
    expireat(bs"key", Int.MaxValue).assert(identity)
  }

  test("OBJECT REFCOUNT") {
    setup(set(bs"key", bs"value"))
    objectRefcount(bs"???").assert(_.isEmpty)
    objectRefcount(bs"key").assert(_.nonEmpty)
  }

  test("OBJECT ENCODING") {
    setup(set(bs"key", bs"value"))
    objectEncoding(bs"???").assert(_.isEmpty)
    objectEncoding(bs"key").assert(_.nonEmpty)
  }

  test("OBJECT IDLETIME") {
    setup(set(bs"key", bs"value"))
    objectIdletime(bs"???").assert(_.isEmpty)
    objectIdletime(bs"key").assert(_.nonEmpty)
  }

  test("PERSIST") {
    setup(set(bs"key", bs"value"))
    persist(bs"key").assertEquals(false)
  }

  test("PEXPIRE") {
    setup(set(bs"key", bs"value"))
    pexpire(bs"key", Int.MaxValue).assert(identity)
  }

  test("PEXPIREAT") {
    setup(set(bs"key", bs"value"))
    pexpireat(bs"key", Int.MaxValue).assert(identity)
  }

  test("PTTL") {
    setup(
      set(bs"key", bs"value"),
      setex(bs"exkey", Int.MaxValue, bs"value")
    )
    pttl(bs"???").assertEquals(Opt.Empty)
    pttl(bs"key").assertEquals(Opt(Opt.Empty))
    pttl(bs"exkey").assert(_.exists(_.nonEmpty))
  }

  test("RENAME") {
    setup(set(bs"key", bs"value"))
    rename(bs"key", keyWithSameSlotAs(bs"key")).exec.futureValue
  }

  test("RENAMENX") {
    setup(set(bs"key", bs"value"))
    renamenx(bs"key", keyWithSameSlotAs(bs"key")).assert(identity)
  }

  test("RESTORE") {
    setup(set(bs"key", bs"value"))
    val dumped = dump(bs"key").exec.futureValue.get
    restore(bs"torestore", 1, dumped).exec.futureValue
  }

  test("SORT") {
    sort(bs"somelist",
      Opt(SelfPattern), Opt(SortLimit(0, 1)), SortOrder.Desc, alpha = true).assert(_.isEmpty)
  }

  test("SORT with STORE") {
    sortStore(bs"somelist", keyWithSameSlotAs(bs"somelist")).assertEquals(0)
  }

  test("TTL") {
    setup(
      set(bs"key", bs"value"),
      setex(bs"exkey", Int.MaxValue, bs"value")
    )
    ttl(bs"???").assertEquals(Opt.Empty)
    ttl(bs"key").assertEquals(Opt(Opt.Empty))
    ttl(bs"exkey").assert(_.exists(_.nonEmpty))
  }

  test("TYPE") {
    setup(set(bs"key", bs"value"))
    `type`(bs"key").assertEquals(RedisType.String)
  }
}

trait NodeKeysApiSuite extends ClusteredKeysApiSuite {
  import RedisCommands._

  private val scanKeys = (0 until 32).map(i => bs"toscan$i")

  test("KEYS") {
    setup(mset(scanKeys.map(k => (k, bs"value"))))
    keys(bs"toscan*").assert(_.toSet == scanKeys.toSet)
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
    del(Seq(bs"key", bs"foo")).assertEquals(1)
  }

  test("MOVE") {
    setup(set(bs"key", bs"value"))
    move(bs"key", 1).assert(identity)
  }

  test("EXISTS multikey") {
    setup(set(bs"key", bs"value"))
    exists(Seq(bs"key", bs"foo")).assertEquals(1)
  }

  test("SORT with GET") {
    sortGet(bs"somelist", Seq(HashFieldPattern(bs"hash", bs"*")),
      Opt(SelfPattern), Opt(SortLimit(0, 1)), SortOrder.Desc, alpha = true).assert(_.isEmpty)
  }

  test("SORT with BY") {
    sort(bs"somelist", by = SelfPattern.opt).assert(_.isEmpty)
    sort(bs"somelist", by = KeyPattern(bs"sth_*").opt).assert(_.isEmpty)
    sort(bs"somelist", by = HashFieldPattern(bs"hash_*", bs"sth_*").opt).assert(_.isEmpty)
  }
}

class RedisClusterKeysApiSuite extends RedisClusterCommandsSuite with ClusteredKeysApiSuite
class RedisNodeKeysApiSuite extends RedisNodeCommandsSuite with NodeKeysApiSuite
class RedisConnectionKeysApiSuite extends RedisConnectionCommandsSuite with NodeKeysApiSuite
