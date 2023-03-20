package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._


trait SetsApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("SADD") {
    sadd("key", Nil).assertEquals(0)
    sadd("key", "a", "b", "c").assertEquals(3)
    sadd("key", "a", "b", "d").assertEquals(1)
    smembers("key").assertEquals(Set("a", "b", "c", "d"))
  }

  apiTest("SCARD") {
    setup(sadd("key", "a", "b", "c"))
    scard("???").assertEquals(0)
    scard("key").assertEquals(3)
  }

  apiTest("SDIFF") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "d"),
      sadd("{key}3", "c", "d")
    )
    sdiff("{key}1", "{key}2", "{key}3").assertEquals(Set("a"))
  }

  apiTest("SDIFFSTORE") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "d"),
      sadd("{key}3", "c", "d")
    )
    sdiffstore("{key}d", "{key}1", "{key}2", "{key}3").assertEquals(1)
    smembers("{key}d").assertEquals(Set("a"))
  }

  apiTest("SINTER") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "a"),
      sadd("{key}3", "c", "a")
    )
    sinter("{key}1", "{key}2", "{key}3").assertEquals(Set("a"))
  }

  apiTest("SINTERSTORE") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "a"),
      sadd("{key}3", "c", "a")
    )
    sinterstore("{key}d", "{key}1", "{key}2", "{key}3").assertEquals(1)
    smembers("{key}d").assertEquals(Set("a"))
  }

  apiTest("SISMEMBER") {
    setup(sadd("key", "a", "b", "c"))
    sismember("???", "a").assertEquals(false)
    sismember("key", "a").assertEquals(true)
    sismember("key", "d").assertEquals(false)
  }

  apiTest("SMEMBERS") {
    setup(sadd("key", "a", "b", "c"))
    smembers("???").assertEquals(Set.empty)
    smembers("key").assertEquals(Set("a", "b", "c"))
  }

  apiTest("SMISMEMBER") {
    setup(sadd("key", "a", "b", "c"))
    smismember("???", "a").assertEquals(Seq(false))
    smismember("key", "a").assertEquals(Seq(true))
    smismember("key", "a", "c").assertEquals(Seq(true, true))
    smismember("key", "a", "d").assertEquals(Seq(true, false))
  }

  apiTest("SMOVE") {
    setup(
      sadd("{key}1", "a", "b"),
      sadd("{key}2", "c", "d")
    )
    smove("{key}1", "{key}2", "?").assertEquals(false)
    smove("{key}1", "{key}2", "a").assertEquals(true)
    smembers("{key}1").assertEquals(Set("b"))
    smembers("{key}2").assertEquals(Set("a", "c", "d"))
  }

  apiTest("SPOP") {
    setup(sadd("key", "a", "b", "c"))
    spop("???").assertEquals(Opt.Empty)
    spop("???", 2).assertEquals(Set.empty)
    spop("key").assert(_.exists(Set("a", "b", "c").contains))
    spop("key", 2).assert(s => s.size == 2 && s.forall(Set("a", "b", "c").contains))
    scard("key").assertEquals(0)
  }

  apiTest("SRANDMEMBER") {
    setup(sadd("key", "a", "b", "c"))
    srandmember("???").assertEquals(Opt.Empty)
    srandmemberDistinct("???", 2).assertEquals(Set.empty)
    srandmember("key").assert(_.exists(Set("a", "b", "c").contains))
    srandmemberDistinct("key", 2).assert(s => s.size == 2 && s.forall(Set("a", "b", "c").contains))
    scard("key").assertEquals(3)
  }

  apiTest("SREM") {
    setup(sadd("key", "a", "b", "c"))
    srem("key", Nil).assertEquals(0)
    srem("???", "a", "b").assertEquals(0)
    srem("key", "a", "d").assertEquals(1)
    scard("key").assertEquals(2)
  }

  apiTest("SSCAN") {
    val scanMembers = (0 to 256).map(i => s"toscan$i").toSet
    setup(sadd("key", scanMembers))
    def sscanCollect(cursor: Cursor, acc: Set[String]): Future[Set[String]] =
      sscan("key", cursor, "toscan*", 4).exec.flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data)
        case (nextCursor, data) => sscanCollect(nextCursor, acc ++ data)
      }
    assert(sscanCollect(Cursor.NoCursor, Set.empty).futureValue == scanMembers)
  }

  apiTest("SUNION") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "d"),
      sadd("{key}3", "c", "e")
    )
    sunion(Nil).assertEquals(Set.empty)
    sunion("{key}1", "{key}2", "{key}3").assertEquals(Set("a", "b", "c", "d", "e"))
  }

  apiTest("SUNIONSTORE") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "d"),
      sadd("{key}3", "c", "e")
    )
    sunionstore("{key}d", "{key}1", "{key}2", "{key}3").assertEquals(5)
    smembers("{key}d").assertEquals(Set("a", "b", "c", "d", "e"))
  }
}
