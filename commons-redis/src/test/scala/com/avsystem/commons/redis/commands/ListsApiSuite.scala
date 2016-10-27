package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._

/**
  * Author: ghik
  * Created: 11/10/16.
  */
trait ListsApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("LINDEX") {
    setup(rpush("key", "a", "b", "c"))
    lindex("???", 2).assertEquals(Opt.Empty)
    lindex("key", 5).assertEquals(Opt.Empty)
    lindex("key", 1).assertEquals("b".opt)
  }

  apiTest("LINSERT") {
    setup(rpush("key", "a", "b", "c"))
    linsert("???", "value", "omg").assertEquals(Opt.Empty)
    linsert("key", "value", "omg").assertEquals(Opt.Empty)
    linsert("key", "a", "aa").assertEquals(4L.opt)
    lrange("key").assertEquals(Seq("a", "aa", "b", "c"))
    linsert("key", "b", "bb", before = true).assertEquals(5L.opt)
    lrange("key").assertEquals(Seq("a", "aa", "bb", "b", "c"))
  }

  apiTest("LLEN") {
    setup(rpush("key", "a", "b", "c"))
    llen("???").assertEquals(0)
    llen("key").assertEquals(3)
  }

  apiTest("LPOP") {
    setup(rpush("key", "a", "b", "c"))
    lpop("???").assertEquals(Opt.Empty)
    lpop("key").assertEquals("a".opt)
  }

  apiTest("LPUSH") {
    lpush("key", "a", "b").assertEquals(2)
    lpush("key", "c", "d").assertEquals(4)
    lrange("key").assertEquals(Seq("d", "c", "b", "a"))
  }

  apiTest("LPUSHX") {
    lpushx("key", "a").assertEquals(0)
    lpush("key", "a").assertEquals(1)
    lpushx("key", "b").assertEquals(2)
    lrange("key").assertEquals(Seq("b", "a"))
  }

  apiTest("LRANGE") {
    setup(rpush("key", "a", "b", "c"))
    lrange("???").assertEquals(Seq.empty)
    lrange("key").assertEquals(Seq("a", "b", "c"))
    lrange("key", 1, -2).assertEquals(Seq("b"))
  }

  apiTest("LREM") {
    setup(rpush("key", "a", "a", "a", "b", "a", "a", "a"))
    lrem("???", "value").assertEquals(0)
    lrem("key", "value").assertEquals(0)
    lrem("key", "a", RemCount.fromHead(2)).assertEquals(2)
    lrange("key").assertEquals(Seq("a", "b", "a", "a", "a"))
    lrem("key", "a", RemCount.fromTail(2)).assertEquals(2)
    lrange("key").assertEquals(Seq("a", "b", "a"))
    lrem("key", "a").assertEquals(2)
    lrange("key").assertEquals(Seq("b"))
  }

  apiTest("LSET") {
    setup(rpush("key", "a", "b", "c"))
    lset("key", 0, "A").get
    lrange("key").assertEquals(Seq("A", "b", "c"))
  }

  apiTest("LTRIM") {
    setup(rpush("key", "a", "b", "c"))
    ltrim("???").get
    ltrim("key").get
    lrange("key").assertEquals(Seq("a", "b", "c"))
    ltrim("key", 0, -2).get
    lrange("key").assertEquals(Seq("a", "b"))
  }

  apiTest("RPOP") {
    setup(rpush("key", "a", "b", "c"))
    rpop("???").assertEquals(Opt.Empty)
    rpop("key").assertEquals("c".opt)
  }

  apiTest("RPOPLPUSH") {
    setup(rpush("{key}1", "a", "b"), rpush("{key}2", "c", "d"))
    rpoplpush("{key}?", "{key}1").assertEquals(Opt.Empty)
    rpoplpush("{key}1", "{key}2").assertEquals("b".opt)
    lrange("{key}1").assertEquals(Seq("a"))
    lrange("{key}2").assertEquals(Seq("b", "c", "d"))
    rpoplpush("{key}2", "{key}3").assertEquals("d".opt)
    lrange("{key}2").assertEquals(Seq("b", "c"))
    lrange("{key}3").assertEquals(Seq("d"))
  }

  apiTest("RPUSH") {
    rpush("key", "a", "b").assertEquals(2)
    rpush("key", "c", "d").assertEquals(4)
    lrange("key").assertEquals(Seq("a", "b", "c", "d"))
  }

  apiTest("RPUSHX") {
    rpushx("key", "a").assertEquals(0)
    rpush("key", "a").assertEquals(1)
    rpushx("key", "b").assertEquals(2)
    lrange("key").assertEquals(Seq("a", "b"))
  }
}

trait BlockingListsApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("BLPOP") {
    setup(
      rpush("key", "a", "b"),
      rpush("{key}1", "a1", "b1"),
      rpush("{key}2", "a2", "b2")
    )
    blpop("key").assertEquals("a")
    blpop("key", 1).assertEquals("b".opt)
    blpop("key", 1).assertEquals(Opt.Empty)
    blpop("{key}1", "{key}2").assertEquals("{key}1", "a1")
    blpop(List("{key}2", "{key}1"), 1).assertEquals(("{key}2", "a2").opt)
    blpop(List("{???}1", "{???}2"), 1).assertEquals(Opt.Empty)
  }

  apiTest("BRPOP") {
    setup(
      rpush("key", "a", "b"),
      rpush("{key}1", "a1", "b1"),
      rpush("{key}2", "a2", "b2")
    )
    brpop("key").assertEquals("b")
    brpop("key", 1).assertEquals("a".opt)
    brpop("key", 1).assertEquals(Opt.Empty)
    brpop("{key}1", "{key}2").assertEquals("{key}1", "b1")
    brpop(List("{key}2", "{key}1"), 1).assertEquals(("{key}2", "b2").opt)
    brpop(List("{???}1", "{???}2"), 1).assertEquals(Opt.Empty)
  }

  apiTest("BRPOPLPUSH") {
    setup(
      rpush("{key}1", "a1", "b1"),
      rpush("{key}2", "a2", "b2")
    )
    brpoplpush("{key}1", "{key}2").assertEquals("b1")
    brpoplpush("{key}1", "{key}2", 1).assertEquals("a1".opt)
    brpoplpush("{key}1", "{key}2", 1).assertEquals(Opt.Empty)
  }
}
