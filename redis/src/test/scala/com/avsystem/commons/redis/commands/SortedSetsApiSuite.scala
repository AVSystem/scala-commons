package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._

trait SortedSetsApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("ZADD") {
    zadd("key", Nil).assertEquals(0)
    zadd("lex", 0.0, "a", "b", "c", "d").assertEquals(4)
    zadd("key", "lol" -> 1.0, "fuu" -> 2.0).assertEquals(2)
    zrangeWithscores("key").assertEquals(Seq("lol" -> 1.0, "fuu" -> 2.0))
    zadd("key", Seq("lol" -> 3.0, "bar" -> 1.0), existence = Existence.XX).assertEquals(0)
    zrangeWithscores("key").assertEquals(Seq("fuu" -> 2.0, "lol" -> 3.0))
    zadd("key", Seq("lol" -> 2.0, "bar" -> 1.0, "omg" -> 4.0), existence = Existence.NX).assertEquals(2)
    zrangeWithscores("key").assertEquals(Seq("bar" -> 1.0, "fuu" -> 2.0, "lol" -> 3.0, "omg" -> 4.0))
    zadd("key", Seq("bar" -> 1.0, "fuu" -> 1.0), changed = true).assertEquals(1)
  }

  apiTest("ZADD with INCR") {
    zaddIncr("key", "value", 1.0, existence = Existence.XX).assertEquals(Opt.Empty)
    zaddIncr("key", "value", 1.0, existence = Existence.NX).assertEquals(1.0.opt)
    zaddIncr("key", "value", 1.0, existence = Existence.XX).assertEquals(2.0.opt)
    zaddIncr("key", "value", 1.0).assertEquals(3.0.opt)
  }

  apiTest("ZCARD") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0))
    zcard("???").assertEquals(0)
    zcard("key").assertEquals(2)
  }

  apiTest("ZCOUNT") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0))
    zcount("???").assertEquals(0)
    zcount("key").assertEquals(2)
    zcount("key", ScoreLimit.incl(1.0), ScoreLimit.excl(2.0)).assertEquals(1)
  }

  apiTest("ZDIFF") {
    setup(
      zadd("{key}1", "lol" -> 1.0, "fuu" -> 2.0, "oof" -> 3.0),
      zadd("{key}2", "lol" -> 1.0, "fag" -> 2.0),
      zadd("{key}3", "fuu" -> 1.0, "fag" -> 2.0),
    )
    zdiff("{key}1", "{key}2").assertEquals(Seq("fuu", "oof"))
    zdiff("{key}1", "{key}2", "{key}3").assertEquals(Seq("oof"))
    zdiffWithscores("{key}1", "{key}2").assertEquals(Seq("fuu" -> 2.0, "oof" -> 3.0))
  }

  apiTest("ZDIFFSTORE") {
    setup(zadd("{key}1", "lol" -> 1.0, "fuu" -> 2.0, "oof" -> 3.0))
    setup(zadd("{key}2", "lol" -> 1.0, "fag" -> 2.0))
    setup(zadd("{key}3", "fuu" -> 1.0, "fag" -> 2.0))
    zdiffstore("{key}3", "{key}1", "{key}2").assertEquals(2)
  }

  apiTest("ZINCRBY") {
    zincrby("key", 1.0, "value").assertEquals(1.0)
    zincrby("key", 1.0, "value").assertEquals(2.0)
  }

  apiTest("ZINTER") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 4.0),
    )
    zinter("{key}1", "{key}2").assertEquals(Seq("bar"))
    zinterWithscores("{key}1", "{key}2").assertEquals(Seq("bar" -> 5.0))
    zinterWithscores(Seq("{key}1", "{key}2"), Aggregation.Sum).assertEquals(Seq("bar" -> 5.0))
    zinterWithscores(Seq("{key}1", "{key}2"), Aggregation.Min).assertEquals(Seq("bar" -> 2.0))
    zinterWithscores(Seq("{key}1", "{key}2"), Aggregation.Max).assertEquals(Seq("bar" -> 3.0))
  }

  apiTest("ZINTER with WEIGHTS") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 4.0),
    )
    zinterWeights("{key}1" -> 1.0, "{key}2" -> 2.0).assertEquals(Seq("bar"))
    zinterWeightsWithscores("{key}1" -> 1.0, "{key}2" -> 2.0).assertEquals(Seq("bar" -> 8.0))
  }

  apiTest("ZINTERSTORE") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 4.0),
    )
    zinterstore("key", "{key}1", "{key}?").assertEquals(0)
    zinterstore("key", "{key}1", "{key}2").assertEquals(1)
    zrangeWithscores("key").assertEquals(Seq("bar" -> 5.0))
    zinterstore("key", Seq("{key}1", "{key}2"), Aggregation.Sum).assertEquals(1)
    zrangeWithscores("key").assertEquals(Seq("bar" -> 5.0))
    zinterstore("key", Seq("{key}1", "{key}2"), Aggregation.Min).assertEquals(1)
    zrangeWithscores("key").assertEquals(Seq("bar" -> 2.0))
    zinterstore("key", Seq("{key}1", "{key}2"), Aggregation.Max).assertEquals(1)
    zrangeWithscores("key").assertEquals(Seq("bar" -> 3.0))
  }

  apiTest("ZINTERSTORE with WEIGHTS") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 4.0),
    )
    zinterstoreWeights("key", "{key}1" -> 1.0, "{key}2" -> 2.0).assertEquals(1)
    zrangeWithscores("key").assertEquals(Seq("bar" -> 8.0))
  }

  apiTest("ZLEXCOUNT") {
    setup(zadd("key", 0.0, "a", "b", "c"))
    zlexcount("???").assertEquals(0)
    zlexcount("key").assertEquals(3)
    zlexcount("key", LexLimit.incl("a"), LexLimit.excl("c")).assertEquals(2)
  }

  apiTest("ZMSCORE") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zmscore("key", "a", "b", "e").assertEquals(Seq(Opt(1.0), Opt(2.0), Opt.Empty))
  }

  apiTest("ZPOPMAX") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0, "bar" -> 3.0, "fag" -> 4.0))
    zpopmax("???").assertEquals(Opt.Empty)
    zpopmax("???", 2).assertEquals(Seq.empty)
    zpopmax("key").assertEquals(Opt("fag" -> 4.0))
    zpopmax("key", 2).assertEquals(Seq("bar" -> 3.0, "fuu" -> 2.0))
  }

  apiTest("ZPOPMIN") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0, "bar" -> 3.0, "fag" -> 4.0))
    zpopmin("???").assertEquals(Opt.Empty)
    zpopmin("???", 2).assertEquals(Seq.empty)
    zpopmin("key").assertEquals(Opt("lol" -> 1.0))
    zpopmin("key", 2).assertEquals(Seq("fuu" -> 2.0, "bar" -> 3.0))
  }

  apiTest("ZRANDMEMBER") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0, "bar" -> 3.0, "fag" -> 4.0))
    zrandmember("key").assert(_.isDefined)
    zrandmember("key", count = 2).assert(_.size == 2)
    zrandmemberWithscores("key", count = 2).assert(_.size == 2)
  }

  apiTest("ZRANGE") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0))
    zrange("???").assertEquals(Seq.empty)
    zrange("key").assertEquals(Seq("lol", "fuu"))
    zrange("key", 1).assertEquals(Seq("fuu"))
    zrange("key", 0, -2).assertEquals(Seq("lol"))
  }

  apiTest("ZRANGE WITHSCORES") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0))
    zrangeWithscores("???").assertEquals(Seq.empty)
    zrangeWithscores("key").assertEquals(Seq("lol" -> 1.0, "fuu" -> 2.0))
    zrangeWithscores("key", 1).assertEquals(Seq("fuu" -> 2.0))
    zrangeWithscores("key", 0, -2).assertEquals(Seq("lol" -> 1.0))
  }

  apiTest("ZRANGEBYLEX") {
    setup(zadd("key", 0.0, "a", "b", "c"))
    zrangebylex("???").assertEquals(Seq.empty)
    zrangebylex("key").assertEquals(Seq("a", "b", "c"))
    zrangebylex("key", limit = Limit(1, 1)).assertEquals(Seq("b"))
    zrangebylex("key", LexLimit.incl("a"), LexLimit.excl("c")).assertEquals(Seq("a", "b"))
  }

  apiTest("ZRANGEBYSCORE") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zrangebyscore("???").assertEquals(Seq.empty)
    zrangebyscore("key").assertEquals(Seq("a", "b", "c", "d"))
    zrangebyscore("key", limit = Limit(1, 2)).assertEquals(Seq("b", "c"))
    zrangebyscore("key", ScoreLimit.incl(1.0), ScoreLimit.excl(3.0)).assertEquals(Seq("a", "b"))
  }

  apiTest("ZRANGEBYSCORE WITHSCORES") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zrangebyscoreWithscores("???").assertEquals(Seq.empty)
    zrangebyscoreWithscores("key").assertEquals(Seq("a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zrangebyscoreWithscores("key", limit = Limit(1, 2)).assertEquals(Seq("b" -> 2.0, "c" -> 3.0))
    zrangebyscoreWithscores("key", ScoreLimit.incl(1.0), ScoreLimit.excl(3.0)).assertEquals(Seq("a" -> 1.0, "b" -> 2.0))
  }

  apiTest("ZRANK") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zrank("???", "value").assertEquals(Opt.Empty)
    zrank("key", "???").assertEquals(Opt.Empty)
    zrank("key", "b").assertEquals(1L.opt)
  }

  apiTest("ZREM") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zrem("key", Nil).assertEquals(0)
    zrem("???", "a", "o").assertEquals(0)
    zrem("key", "a", "o").assertEquals(1)
  }

  apiTest("ZREMRANGEBYLEX") {
    setup(zadd("key", 0.0, "a", "b", "c", "d"))
    zremrangebylex("???").assertEquals(0)
    zremrangebylex("key", LexLimit.incl("a"), LexLimit.excl("c")).assertEquals(2)
    zremrangebylex("key").assertEquals(2)
    exists("key").assertEquals(false)
  }

  apiTest("ZREMRANGEBYRANK") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zremrangebyrank("???").assertEquals(0)
    zremrangebyrank("key", 0, 1).assertEquals(2)
    zremrangebyrank("key").assertEquals(2)
    exists("key").assertEquals(false)
  }

  apiTest("ZREMRANGEBYSCORE") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zremrangebyscore("???").assertEquals(0)
    zremrangebyscore("key", ScoreLimit.incl(1.0), ScoreLimit.excl(3.0)).assertEquals(2)
    zremrangebyscore("key").assertEquals(2)
    exists("key").assertEquals(false)
  }

  apiTest("ZREVRANGE") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0))
    zrevrange("???").assertEquals(Seq.empty)
    zrevrange("key").assertEquals(Seq("fuu", "lol"))
    zrevrange("key", 1).assertEquals(Seq("lol"))
    zrevrange("key", 0, -2).assertEquals(Seq("fuu"))
  }

  apiTest("ZREVRANGE WITHSCORES") {
    setup(zadd("key", "lol" -> 1.0, "fuu" -> 2.0))
    zrevrangeWithscores("???").assertEquals(Seq.empty)
    zrevrangeWithscores("key").assertEquals(Seq("fuu" -> 2.0, "lol" -> 1.0))
    zrevrangeWithscores("key", 1).assertEquals(Seq("lol" -> 1.0))
    zrevrangeWithscores("key", 0, -2).assertEquals(Seq("fuu" -> 2.0))
  }

  apiTest("ZREVRANGEBYLEX") {
    setup(zadd("key", 0.0, "a", "b", "c"))
    zrevrangebylex("???").assertEquals(Seq.empty)
    zrevrangebylex("key").assertEquals(Seq("c", "b", "a"))
    zrevrangebylex("key", limit = Limit(1, 1)).assertEquals(Seq("b"))
    zrevrangebylex("key", LexLimit.excl("c"), LexLimit.incl("a")).assertEquals(Seq("b", "a"))
  }

  apiTest("ZREVRANGEBYSCORE") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zrevrangebyscore("???").assertEquals(Seq.empty)
    zrevrangebyscore("key").assertEquals(Seq("d", "c", "b", "a"))
    zrevrangebyscore("key", limit = Limit(1, 2)).assertEquals(Seq("c", "b"))
    zrevrangebyscore("key", ScoreLimit.excl(3.0), ScoreLimit.incl(1.0)).assertEquals(Seq("b", "a"))
  }

  apiTest("ZREVRANGEBYSCORE WITHSCORES") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zrevrangebyscoreWithscores("???").assertEquals(Seq.empty)
    zrevrangebyscoreWithscores("key").assertEquals(Seq("d" -> 4.0, "c" -> 3.0, "b" -> 2.0, "a" -> 1.0))
    zrevrangebyscoreWithscores("key", limit = Limit(1, 2)).assertEquals(Seq("c" -> 3.0, "b" -> 2.0))
    zrevrangebyscoreWithscores("key", ScoreLimit.excl(3.0), ScoreLimit.incl(1.0))
      .assertEquals(Seq("b" -> 2.0, "a" -> 1.0))
  }

  apiTest("ZREVRANK") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zrevrank("???", "value").assertEquals(Opt.Empty)
    zrevrank("key", "???").assertEquals(Opt.Empty)
    zrevrank("key", "b").assertEquals(2L.opt)
  }

  apiTest("ZSCAN") {
    val scanMembers = (0 until 256).map(i => (s"toscan$i", i.toDouble))
    setup(zadd("key", scanMembers))
    zscan("???", Cursor.NoCursor).assertEquals((Cursor.NoCursor, Seq.empty))
    def zscanCollect(cursor: Cursor, acc: Seq[(String, Double)]): Future[Seq[(String, Double)]] =
      zscan("key", cursor, "toscan*", 2).exec.flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data)
        case (nextCursor, data) => zscanCollect(nextCursor, acc ++ data)
      }
    zscanCollect(Cursor.NoCursor, Vector.empty).futureValue.sortBy(_._2.toInt) shouldEqual scanMembers
  }

  apiTest("ZSCORE") {
    setup(zadd("key", "a" -> 1.0, "b" -> 2.0, "c" -> 3.0, "d" -> 4.0))
    zscore("???", "a").assertEquals(Opt.Empty)
    zscore("key", "???").assertEquals(Opt.Empty)
    zscore("key", "b").assertEquals(2.0.opt)
  }

  apiTest("ZUNION") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 6.0),
    )
    zunion("{key}1", "{key}2").assertEquals(Seq("foo", "bar", "lol"))
    zunionWithscores("{key}1", "{key}2").assertEquals(Seq("foo" -> 1.0, "bar" -> 5.0, "lol" -> 6.0))
    zunionWithscores(Seq("{key}1", "{key}2"), Aggregation.Sum).assertEquals(Seq("foo" -> 1.0, "bar" -> 5.0, "lol" -> 6.0))
    zunionWithscores(Seq("{key}1", "{key}2"), Aggregation.Min).assertEquals(Seq("foo" -> 1.0, "bar" -> 2.0, "lol" -> 6.0))
    zunionWithscores(Seq("{key}1", "{key}2"), Aggregation.Max).assertEquals(Seq("foo" -> 1.0, "bar" -> 3.0, "lol" -> 6.0))
  }

  apiTest("ZUNION with WEIGHTS") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 5.0),
    )
    zunionWeights("{key}1" -> 1.0, "{key}2" -> 2.0).assertEquals(Seq("foo", "bar", "lol"))
    zunionWeightsWithscores("{key}1" -> 1.0, "{key}2" -> 2.0).assertEquals(Seq("foo" -> 1.0, "bar" -> 8.0, "lol" -> 10.0))
  }

  apiTest("ZUNIONSTORE") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 4.0),
    )
    zunionstore("key", "{key}1", "{key}?").assertEquals(2)
    zunionstore("key", "{key}1", "{key}2").assertEquals(3)
    zrangeWithscores("key").assertEquals(Seq("foo" -> 1.0, "lol" -> 4.0, "bar" -> 5.0))
    zunionstore("key", Seq("{key}1", "{key}2"), Aggregation.Sum).assertEquals(3)
    zrangeWithscores("key").assertEquals(Seq("foo" -> 1.0, "lol" -> 4.0, "bar" -> 5.0))
    zunionstore("key", Seq("{key}1", "{key}2"), Aggregation.Min).assertEquals(3)
    zrangeWithscores("key").assertEquals(Seq("foo" -> 1.0, "bar" -> 2.0, "lol" -> 4.0))
    zunionstore("key", Seq("{key}1", "{key}2"), Aggregation.Max).assertEquals(3)
    zrangeWithscores("key").assertEquals(Seq("foo" -> 1.0, "bar" -> 3.0, "lol" -> 4.0))
  }

  apiTest("ZUNIONSTORE with WEIGHTS") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 4.0),
    )
    zunionstoreWeights("key", "{key}1" -> 1.0, "{key}2" -> 2.0).assertEquals(3)
    zrangeWithscores("key").assertEquals(Seq("foo" -> 1.0, "bar" -> 8.0, "lol" -> 8.0))
  }

  apiTest("BZPOPMAX") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 4.0),
    )
    bzpopmax("???", 1).assertEquals(Opt.Empty)
    bzpopmax(List("{key}0", "{key}1", "{key}2"), 1).assertEquals(Opt("{key}1", "bar", 2.0))
  }

  apiTest("BZPOPMIN") {
    setup(
      zadd("{key}1", "foo" -> 1.0, "bar" -> 2.0),
      zadd("{key}2", "bar" -> 3.0, "lol" -> 4.0),
    )
    bzpopmin("???", 1).assertEquals(Opt.Empty)
    bzpopmin(List("{key}0", "{key}1", "{key}2"), 1).assertEquals(Opt("{key}1", "foo", 1.0))
  }
}
