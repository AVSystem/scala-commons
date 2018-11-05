package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.ClusterUtils.keyWithSameSlotAs
import com.avsystem.commons.redis._


/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait KeyedKeysApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("DEL") {
    setup(mset("{key}1" -> "value", "{key}2" -> "value"))
    del(Nil).assertEquals(0)
    del("???").assertEquals(false)
    del("{key}1").assertEquals(true)
    del("{key}2", "{key}?").assertEquals(1)
  }

  apiTest("DUMP") {
    setup(set("key", "value"))
    dump("???").assert(_.isEmpty)
    dump("key").assert(_.nonEmpty)
  }

  apiTest("EXISTS") {
    setup(mset("{key}1" -> "value", "{key}2" -> "value"))
    exists(Nil).assertEquals(0)
    exists("???").assertEquals(false)
    exists("{key}1").assertEquals(true)
    exists("{key}2", "{key}?").assertEquals(1)
  }

  apiTest("EXPIRE") {
    setup(set("key", "value"))
    expire("key", Int.MaxValue).assert(identity)
  }

  apiTest("EXPIREAT") {
    setup(set("key", "value"))
    expireat("key", Int.MaxValue).assert(identity)
  }

  apiTest("OBJECT REFCOUNT") {
    setup(set("key", "value"))
    objectRefcount("???").assert(_.isEmpty)
    objectRefcount("key").assert(_.nonEmpty)
  }

  apiTest("OBJECT ENCODING") {
    setup(set("key", "value"))
    objectEncoding("???").assert(_.isEmpty)
    objectEncoding("key").assert(_.nonEmpty)
  }

  apiTest("OBJECT IDLETIME") {
    setup(set("key", "value"))
    objectIdletime("???").assert(_.isEmpty)
    objectIdletime("key").assert(_.nonEmpty)
  }

  apiTest("MEMORY USAGE") {
    setup(set("key", "value"))
    memoryUsage("???").assertEquals(Opt.Empty)
    memoryUsage("key").assert(_.exists(_ > 5))
    memoryUsage("key", 0L).assert(_.exists(_ > 5))
    memoryUsage("key", 1L).assert(_.exists(_ > 5))
  }

  apiTest("PERSIST") {
    setup(set("key", "value"))
    persist("key").assertEquals(false)
  }

  apiTest("PEXPIRE") {
    setup(set("key", "value"))
    pexpire("key", Int.MaxValue).assert(identity)
  }

  apiTest("PEXPIREAT") {
    setup(set("key", "value"))
    pexpireat("key", Int.MaxValue).assert(identity)
  }

  apiTest("PTTL") {
    setup(
      set("key", "value"),
      setex("exkey", Int.MaxValue, "value")
    )
    pttl("???").assertEquals(Opt.Empty)
    pttl("key").assertEquals(Opt(Opt.Empty))
    pttl("exkey").assert(_.exists(_.nonEmpty))
  }

  apiTest("RENAME") {
    setup(set("key", "value"))
    rename("key", keyWithSameSlotAs("key")).exec.futureValue
  }

  apiTest("RENAMENX") {
    setup(set("key", "value"))
    renamenx("key", keyWithSameSlotAs("key")).assert(identity)
  }

  apiTest("RESTORE") {
    setup(set("key", "value"))
    val dumped = dump("key").exec.futureValue.get
    restore("torestore", 1, dumped).exec.futureValue
  }

  apiTest("SORT") {
    sort("somelist",
      SelfPattern, SortLimit(0, 1), SortOrder.Desc, alpha = true).assert(_.isEmpty)
  }

  apiTest("SORT with STORE") {
    sortStore("somelist", keyWithSameSlotAs("somelist")).assertEquals(0)
  }

  apiTest("TOUCH") {
    setup(mset("{key}1" -> "value", "{key}2" -> "value"))
    touch(Nil).assertEquals(0)
    touch("???").assertEquals(false)
    touch("{key}1").assertEquals(true)
    touch("{key}2", "{key}?").assertEquals(1)
  }

  apiTest("TTL") {
    setup(
      set("key", "value"),
      setex("exkey", Int.MaxValue, "value")
    )
    ttl("???").assertEquals(Opt.Empty)
    ttl("key").assertEquals(Opt(Opt.Empty))
    ttl("exkey").assert(_.exists(_.nonEmpty))
  }

  apiTest("TYPE") {
    setup(set("key", "value"))
    `type`("key").assertEquals(RedisType.String)
  }

  apiTest("UNLINK") {
    setup(mset("{key}1" -> "value", "{key}2" -> "value"))
    unlink(Nil).assertEquals(0)
    unlink("???").assertEquals(false)
    unlink("{key}1").assertEquals(true)
    unlink("{key}2", "{key}?").assertEquals(1)
  }
}

trait NodeKeysApiSuite extends KeyedKeysApiSuite {

  import RedisApi.Batches.StringTyped._

  private val scanKeys = (0 until 256).map(i => s"toscan$i")

  apiTest("KEYS") {
    setup(mset(scanKeys.map(k => (k, "value"))))
    keys("toscan*").assert(_.toSet == scanKeys.toSet)
  }

  apiTest("SCAN") {
    setup(mset(scanKeys.map(k => (k, "value"))))
    def scanCollect(cursor: Cursor, acc: Seq[String]): Future[Seq[String]] =
      scan(cursor, "toscan*", 4L).exec.flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data)
        case (nextCursor, data) => scanCollect(nextCursor, acc ++ data)
      }
    assert(scanCollect(Cursor.NoCursor, Vector.empty).futureValue.toSet == scanKeys.toSet)
  }

  apiTest("MOVE") {
    setup(set("key", "value"))
    move("key", 1).assert(identity)
  }

  apiTest("SORT with GET") {
    sortGet("somelist", Seq(FieldPattern("hash", "*")),
      SelfPattern, SortLimit(0, 1), SortOrder.Desc, alpha = true).assert(_.isEmpty)
  }

  apiTest("SORT with BY") {
    sort("somelist", by = SelfPattern).assert(_.isEmpty)
    sort("somelist", by = KeyPattern("sth_*")).assert(_.isEmpty)
    sort("somelist", by = FieldPattern("hash_*", "sth_*")).assert(_.isEmpty)
  }
}
