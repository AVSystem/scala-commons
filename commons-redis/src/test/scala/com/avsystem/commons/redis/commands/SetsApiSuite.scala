package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._

import scala.concurrent.Future

trait SetsApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("SADD") {
    sadd("key", "a", "b", "c").assertEquals(3)
    sadd("key", "a", "b", "d").assertEquals(1)
    smembers("key").map(_.toSet).assertEquals(Set("a", "b", "c", "d"))
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
    sdiff("{key}1", "{key}2", "{key}3").assertEquals(Seq("a"))
  }

  apiTest("SDIFFSTORE") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "d"),
      sadd("{key}3", "c", "d")
    )
    sdiffstore("{key}d", "{key}1", "{key}2", "{key}3").assertEquals(1)
    smembers("{key}d").assertEquals(Seq("a"))
  }

  apiTest("SINTER") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "a"),
      sadd("{key}3", "c", "a")
    )
    sinter("{key}1", "{key}2", "{key}3").assertEquals(Seq("a"))
  }

  apiTest("SINTERSTORE") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "a"),
      sadd("{key}3", "c", "a")
    )
    sinterstore("{key}d", "{key}1", "{key}2", "{key}3").assertEquals(1)
    smembers("{key}d").assertEquals(Seq("a"))
  }

  apiTest("SISMEMBER") {
    setup(sadd("key", "a", "b", "c"))
    sismember("???", "a").assertEquals(false)
    sismember("key", "a").assertEquals(true)
    sismember("key", "d").assertEquals(false)
  }

  apiTest("SMEMBERS") {
    setup(sadd("key", "a", "b", "c"))
    smembers("???").assertEquals(Seq.empty)
    smembers("key").map(_.toSet).assertEquals(Set("a", "b", "c"))
  }

  apiTest("SMOVE") {
    setup(
      sadd("{key}1", "a", "b"),
      sadd("{key}2", "c", "d")
    )
    smove("{key}1", "{key}2", "?").assertEquals(false)
    smove("{key}1", "{key}2", "a").assertEquals(true)
    smembers("{key}1").assertEquals(Seq("b"))
    smembers("{key}2").map(_.toSet).assertEquals(Set("a", "c", "d"))
  }

  apiTest("SPOP") {
    setup(sadd("key", "a", "b", "c"))
    spop("???").assertEquals(Opt.Empty)
    spop("???", 2).assertEquals(Seq.empty)
    spop("key").assert(_.exists(Set("a", "b", "c").contains))
    spop("key", 2).assert(s => s.size == 2 && s.forall(Set("a", "b", "c").contains))
    scard("key").assertEquals(0)
  }

  apiTest("SRANDMEMBER") {
    setup(sadd("key", "a", "b", "c"))
    srandmember("???").assertEquals(Opt.Empty)
    srandmember("???", 2).assertEquals(Seq.empty)
    srandmember("key").assert(_.exists(Set("a", "b", "c").contains))
    srandmember("key", 2).assert(s => s.size == 2 && s.forall(Set("a", "b", "c").contains))
    scard("key").assertEquals(3)
  }

  apiTest("SREM") {
    setup(sadd("key", "a", "b", "c"))
    srem("???", "a", "b").assertEquals(0)
    srem("key", "a", "d").assertEquals(1)
    scard("key").assertEquals(2)
  }

  apiTest("SSCAN") {
    val scanMembers = (0 to 256).map(i => s"toscan$i")
    setup(sadd("key", scanMembers: _*))
    def sscanCollect(cursor: Cursor, acc: Seq[String]): Future[Seq[String]] =
      sscan("key", cursor, "toscan*", 4L).exec.flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data)
        case (nextCursor, data) => sscanCollect(nextCursor, acc ++ data)
      }
    assert(sscanCollect(Cursor.NoCursor, Vector.empty).futureValue.toSet == scanMembers.toSet)
  }

  apiTest("SUNION") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "d"),
      sadd("{key}3", "c", "e")
    )
    sunion("{key}1", "{key}2", "{key}3").map(_.toSet).assertEquals(Set("a", "b", "c", "d", "e"))
  }

  apiTest("SUNIONSTORE") {
    setup(
      sadd("{key}1", "a", "b", "c"),
      sadd("{key}2", "b", "d"),
      sadd("{key}3", "c", "e")
    )
    sunionstore("{key}d", "{key}1", "{key}2", "{key}3").assertEquals(5)
    smembers("{key}d").map(_.toSet).assertEquals(Set("a", "b", "c", "d", "e"))
  }
}
