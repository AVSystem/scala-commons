package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.ClusterUtils.keyWithSameSlotAs
import com.avsystem.commons.redis._

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait ClusteredKeysApiSuite extends CommandsSuite {
  import RedisStringCommands._

  test("DEL") {
    setup(set("key", "value"))
    del(Seq("key")).assertEquals(1)
  }

  test("DUMP") {
    setup(set("key", "value"))
    dump("???").assert(_.isEmpty)
    dump("key").assert(_.nonEmpty)
  }

  test("EXISTS") {
    setup(set("key", "value"))
    exists(Seq("???")).assertEquals(0)
    exists(Seq("key")).assertEquals(1)
  }

  test("EXPIRE") {
    setup(set("key", "value"))
    expire("key", Int.MaxValue).assert(identity)
  }

  test("EXPIREAT") {
    setup(set("key", "value"))
    expireat("key", Int.MaxValue).assert(identity)
  }

  test("OBJECT REFCOUNT") {
    setup(set("key", "value"))
    objectRefcount("???").assert(_.isEmpty)
    objectRefcount("key").assert(_.nonEmpty)
  }

  test("OBJECT ENCODING") {
    setup(set("key", "value"))
    objectEncoding("???").assert(_.isEmpty)
    objectEncoding("key").assert(_.nonEmpty)
  }

  test("OBJECT IDLETIME") {
    setup(set("key", "value"))
    objectIdletime("???").assert(_.isEmpty)
    objectIdletime("key").assert(_.nonEmpty)
  }

  test("PERSIST") {
    setup(set("key", "value"))
    persist("key").assertEquals(false)
  }

  test("PEXPIRE") {
    setup(set("key", "value"))
    pexpire("key", Int.MaxValue).assert(identity)
  }

  test("PEXPIREAT") {
    setup(set("key", "value"))
    pexpireat("key", Int.MaxValue).assert(identity)
  }

  test("PTTL") {
    setup(
      set("key", "value"),
      setex("exkey", Int.MaxValue, "value")
    )
    pttl("???").assertEquals(Opt.Empty)
    pttl("key").assertEquals(Opt(Opt.Empty))
    pttl("exkey").assert(_.exists(_.nonEmpty))
  }

  test("RENAME") {
    setup(set("key", "value"))
    rename("key", keyWithSameSlotAs("key")).exec.futureValue
  }

  test("RENAMENX") {
    setup(set("key", "value"))
    renamenx("key", keyWithSameSlotAs("key")).assert(identity)
  }

  test("RESTORE") {
    setup(set("key", "value"))
    val dumped = dump("key").exec.futureValue.get
    restore("torestore", 1, dumped).exec.futureValue
  }

  test("SORT") {
    sort("somelist",
      Opt(SelfPattern), Opt(SortLimit(0, 1)), SortOrder.Desc, alpha = true).assert(_.isEmpty)
  }

  test("SORT with STORE") {
    sortStore("somelist", keyWithSameSlotAs("somelist")).assertEquals(0)
  }

  test("TTL") {
    setup(
      set("key", "value"),
      setex("exkey", Int.MaxValue, "value")
    )
    ttl("???").assertEquals(Opt.Empty)
    ttl("key").assertEquals(Opt(Opt.Empty))
    ttl("exkey").assert(_.exists(_.nonEmpty))
  }

  test("TYPE") {
    setup(set("key", "value"))
    `type`("key").assertEquals(RedisType.String)
  }
}

trait NodeKeysApiSuite extends ClusteredKeysApiSuite {
  import RedisStringCommands._

  private val scanKeys = (0 until 32).map(i => s"toscan$i")

  test("KEYS") {
    setup(mset(scanKeys.map(k => (k, "value"))))
    keys("toscan*").assert(_.toSet == scanKeys.toSet)
  }

  test("SCAN") {
    setup(mset(scanKeys.map(k => (k, "value"))))
    def scanCollect(cursor: Cursor, acc: Seq[String]): Future[Seq[String]] =
      scan(cursor, Opt("toscan*"), Opt(4L)).exec.flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data)
        case (nextCursor, data) => scanCollect(nextCursor, acc ++ data)
      }
    assert(scanCollect(Cursor.NoCursor, Vector.empty).futureValue.toSet == scanKeys.toSet)
  }

  test("DEL multikey") {
    setup(set("key", "value"))
    del(Seq("key", "foo")).assertEquals(1)
  }

  test("MOVE") {
    setup(set("key", "value"))
    move("key", 1).assert(identity)
  }

  test("EXISTS multikey") {
    setup(set("key", "value"))
    exists(Seq("key", "foo")).assertEquals(1)
  }

  test("SORT with GET") {
    sortGet("somelist", Seq(HashFieldPattern("hash", "*")),
      Opt(SelfPattern), Opt(SortLimit(0, 1)), SortOrder.Desc, alpha = true).assert(_.isEmpty)
  }

  test("SORT with BY") {
    sort("somelist", by = SelfPattern.opt).assert(_.isEmpty)
    sort("somelist", by = KeyPattern("sth_*").opt).assert(_.isEmpty)
    sort("somelist", by = HashFieldPattern("hash_*", "sth_*").opt).assert(_.isEmpty)
  }
}

class RedisClusterKeysApiSuite extends RedisClusterCommandsSuite with ClusteredKeysApiSuite
class RedisNodeKeysApiSuite extends RedisNodeCommandsSuite with NodeKeysApiSuite
class RedisConnectionKeysApiSuite extends RedisConnectionCommandsSuite with NodeKeysApiSuite
