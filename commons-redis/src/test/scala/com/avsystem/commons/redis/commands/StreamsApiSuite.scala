package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.{CommandsSuite, RedisApi, RedisBatch}

trait StreamsApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped.{XEntry => Entry, _}

  def id(i: Int): XEntryId = XEntryId(0, i.toLong)
  def ids(from: Int, to: Int): Seq[XEntryId] = (from to to).map(id)
  def entry(i: Int): Entry = Entry(id(i), Map("f" -> s"v$i"))
  def entries(from: Int, to: Int): Seq[Entry] = (from to to).map(entry)

  apiTest("XACK") {
    setup(
      xgroupCreate("key", XGroup("g"), mkstream = true),
      RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))),
      xreadgroupSingle("key", XGroup("g"), XConsumer("c"))
    )
    xack("key", XGroup("g"), id(1)).assertEquals(true)
    xack("key", XGroup("g"), id(1)).assertEquals(false)
    xack("key", XGroup("g"), id(1), id(2), id(3)).assertEquals(2)
    xack("key", XGroup("g"), (1 to 10).map(id)).assertEquals(7)
  }

  apiTest("XADD") {
    xadd("key", "f" -> "v").get
    val lastId = xadd("key", "f1" -> "v1", "f2" -> "v2").get
    xlen("key").assertEquals(2)
    xadd("key", List("f1" -> "v1", "f2" -> "v2"), lastId.inc, XMaxlen(1)).assertEquals(lastId.inc)
    xlen("key").assert(_ >= 1)
    val entry = XEntry(lastId.inc.inc, Map("f1" -> "v1", "f2" -> "v2"))
    xaddEntry("key", entry, XMaxlen(1, approx = false)).assertEquals(lastId.inc.inc)
    xlen("key").assertEquals(1)
  }

  apiTest("XCLAIM") {
    setup(
      xgroupCreate("key", XGroup("g"), mkstream = true),
      RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))),
      xreadgroupSingle("key", XGroup("g"), XConsumer("c1"), count = 5)
    )
    // nonexistent id
    xclaimSingle("key", XGroup("g"), XConsumer("c1"), 0, id(0)).assertEquals(Opt.Empty)
    // existing id already claimed by self
    xclaimSingle("key", XGroup("g"), XConsumer("c1"), 0, id(1)).assertEquals(Opt(entry(1)))
    // existing id claimed by someone else
    xclaimSingle("key", XGroup("g"), XConsumer("c2"), 0, id(1)).assertEquals(Opt(entry(1)))
    // maxIdleTime not exceeded
    xclaimSingle("key", XGroup("g"), XConsumer("c1"), 5000, id(1)).assertEquals(Opt.Empty)
    // force set maxIdleTime and claim again
    xclaimSingle("key", XGroup("g"), XConsumer("c1"), 0, id(1), idleMillis = 10000).assertEquals(Opt(entry(1)))
    xclaimSingle("key", XGroup("g"), XConsumer("c2"), 5000, id(1)).assertEquals(Opt(entry(1)))
    // force set msUnixTime and claim again
    xclaimSingle("key", XGroup("g"), XConsumer("c2"), 0, id(1), msUnixTime = 0).assertEquals(Opt(entry(1)))
    xclaimSingle("key", XGroup("g"), XConsumer("c1"), 5000, id(1)).assertEquals(Opt(entry(1)))
    // retrycount
    xclaimSingle("key", XGroup("g"), XConsumer("c2"), 0, id(1), retrycount = 42).assertEquals(Opt(entry(1)))
    xpendingEntries("key", XGroup("g"), 1, id(1)).assert(_.headOpt.exists(_.deliveredCount == 42))
    // unclaimed id
    xclaimSingle("key", XGroup("g"), XConsumer("c1"), 0, id(6)).assertEquals(Opt.Empty)
    xclaimSingle("key", XGroup("g"), XConsumer("c2"), 0, id(6), force = true).assertEquals(Opt(entry(6)))
    // multiple ids
    xclaim("key", XGroup("g"), XConsumer("c1"), 0, ids(1, 10)).assertEquals(entries(1, 6))
    xclaimJustid("key", XGroup("g"), XConsumer("c1"), 0, ids(1, 10)).assertEquals(ids(1, 6))
    xclaimJustid("key", XGroup("g"), XConsumer("c1"), 0, ids(1, 10), force = true).assertEquals(ids(1, 10))
  }

  apiTest("XDEL") {
    setup(RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))))
    xdel("key", id(0)).assertEquals(false)
    xdel("key", id(1)).assertEquals(true)
    xdel("key", id(1), id(2), id(3)).assertEquals(2)
    xdel("key", ids(1, 10)).assertEquals(7)
  }

  apiTest("XGROUP") {
    xgroupCreate("key", XGroup("g1"), mkstream = true).get
    xgroupCreate("key", XGroup("g2"), id(1)).get
    xgroupSetid("key", XGroup("g2"), id(2)).get
    xgroupDestroy("key", XGroup("g1")).assertEquals(true)
    xgroupDestroy("key", XGroup("g3")).assertEquals(false)
    xgroupDelconsumer("key", XGroup("g2"), XConsumer("c")).assertEquals(false)
    xaddEntry("key", entry(3)).get
    xreadgroupSingle("key", XGroup("g2"), XConsumer("c")).get
    xgroupDelconsumer("key", XGroup("g2"), XConsumer("c")).assertEquals(true)
  }

  apiTest("XINFO STREAM") {
    setup(
      xaddEntry("key", entry(1)),
      xgroupCreate("key", XGroup("g"))
    )
    val xsi = xinfoStream("key").get
    assert(xsi.length == 1)
    assert(xsi.groups == 1)
    assert(xsi.firstEntry == entry(1))
    assert(xsi.lastEntry == entry(1))
    assert(xsi.lastGeneratedId == id(1))
  }

  apiTest("XINFO GROUPS") {
    setup(xgroupCreate("key", XGroup("g"), mkstream = true))
    val Seq(xgi) = xinfoGroups("key").get
    assert(xgi.name == XGroup("g"))
    assert(xgi.consumers == 0)
    assert(xgi.pending == 0)
    assert(xgi.lastDeliveredId == id(0))
  }

  apiTest("XINFO CONSUMERS") {
    setup(
      xgroupCreate("key", XGroup("g"), mkstream = true),
      xaddEntry("key", entry(1)),
      xreadgroupSingle("key", XGroup("g"), XConsumer("c"))
    )
    val Seq(xci) = xinfoConsumers("key", XGroup("g")).get
    assert(xci.name == XConsumer("c"))
    assert(xci.pending == 1)
  }

  apiTest("XLEN") {
    setup(RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))))
    xlen("key").assertEquals(10)
  }

  apiTest("XPENDING") {
    setup(
      xgroupCreate("key", XGroup("g"), mkstream = true),
      RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))),
      xreadgroupSingle("key", XGroup("g"), XConsumer("c"))
    )
    xpending("key", XGroup("g")).assertEquals(XPendingOverview(10, id(1), id(10), Map(XConsumer("c") -> 10)))
    val Seq(xpe) = xpendingEntries("key", XGroup("g"), 1).get
    assert(xpe.id == id(1))
    assert(xpe.consumer == XConsumer("c"))
    assert(xpe.deliveredCount == 1)
    xpendingEntries("key", XGroup("g"), 5, id(3), id(9), XConsumer("c")).map(_.map(_.id)).assertEquals(ids(3, 7))
    xpendingEntries("key", XGroup("g"), 5, id(3), id(9), XConsumer("c2")).assertEquals(Seq.empty)
  }

  apiTest("XRANGE") {
    setup(RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))))
    xrange("key").assertEquals(entries(1, 10))
    xrange("key", start = id(3)).assertEquals(entries(3, 10))
    xrange("key", end = id(3)).assertEquals(entries(1, 3))
    xrange("key", start = id(3), end = id(7)).assertEquals(entries(3, 7))
    xrange("key", start = id(3), end = id(7), count = 2).assertEquals(entries(3, 4))
  }

  apiTest("XREAD") {
    setup(RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))))
    xreadSingle("key", Opt.Empty).assertEquals(Seq.empty)
    xreadSingle("key", id(0).opt).assertEquals(entries(1, 10))
    xreadSingle("key", id(5).opt).assertEquals(entries(6, 10))
    xreadSingle("key", id(5).opt, count = 2).assertEquals(entries(6, 7))
    xreadSingle("key", Opt.Empty, blockMillis = 10).assertEquals(Seq.empty)
    xread(List("key" -> id(0).opt)).assertEquals(Map("key" -> entries(1, 10)))
  }

  apiTest("XREADGROUP") {
    setup(
      xgroupCreate("key", XGroup("g"), mkstream = true),
      RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i)))
    )
    xreadgroupSingle("key", XGroup("g"), XConsumer("c"), count = 5).assertEquals(entries(1, 5))
    xreadgroupSingle("key", XGroup("g"), XConsumer("c")).assertEquals(entries(6, 10))
    xreadgroupSingle("key", XGroup("g"), XConsumer("c")).assertEquals(Seq.empty)
    xreadgroupSingle("key", XGroup("g"), XConsumer("c"), id(5)).assertEquals(entries(6, 10))
    xreadgroupSingle("key", XGroup("g"), XConsumer("c"), id(5), count = 10, blockMillis = 1000).assertEquals(entries(6, 10))
    xreadgroupSingle("key", XGroup("g"), XConsumer("c"), blockMillis = 10).assertEquals(Seq.empty)
    xreadgroup(XGroup("g"), XConsumer("c"), List("key" -> id(5).opt)).assertEquals(Map("key" -> entries(6, 10)))
  }

  apiTest("XREVRANGE") {
    setup(RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))))
    xrevrange("key").assertEquals(entries(1, 10).reverse)
    xrevrange("key", start = id(3)).assertEquals(entries(3, 10).reverse)
    xrevrange("key", end = id(3)).assertEquals(entries(1, 3).reverse)
    xrevrange("key", end = id(7), start = id(3)).assertEquals(entries(3, 7).reverse)
    xrevrange("key", end = id(7), start = id(3), count = 2).assertEquals(entries(6, 7).reverse)
  }

  apiTest("XTRIM") {
    setup(RedisBatch.traverse(1 to 10)(i => xaddEntry("key", entry(i))))
    xtrim("key", 5, approx = false).assertEquals(5)
    xtrim("key", 4).assertEquals(0)
    xtrim("key", XMaxlen(4)).assertEquals(0)
    xtrim("key", XMaxlen(4, approx = false)).assertEquals(1)
  }
}
