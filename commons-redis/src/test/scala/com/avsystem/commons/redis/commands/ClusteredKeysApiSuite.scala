package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.ClusterUtils.keyWithSameSlotAs
import com.avsystem.commons.redis.{CommandsSuite, RedisBatch, RedisClusterCommandsSuite, RedisCommands, RedisConnectionCommandsSuite, RedisNodeCommandsSuite}

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait ClusteredKeysApiSuite extends CommandsSuite {
  type Api <: ClusteredKeysApi

  // only to make IntelliJ happy
  private lazy val cmds: ClusteredKeysApi {type Result[+A, -S] = Future[A]} = commands

  import cmds._

  override def setupCommands = {
    import RedisCommands._
    super.setupCommands *>
      set(bs"key", bs"value") *>
      setex(bs"exkey", Int.MaxValue, bs"value") *>
      set(bs"toex", bs"value") *>
      set(bs"todel", bs"value") *>
      set(bs"torename", bs"value") *>
      set(bs"torenamenx", bs"value")
  }

  test("DEL") {
    assert(del(Seq(bs"todel")).futureValue == 1)
  }

  test("DUMP") {
    assert(dump(bs"???").futureValue.isEmpty)
    assert(dump(bs"key").futureValue.nonEmpty)
  }

  test("EXISTS") {
    assert(exists(Seq(bs"key")).futureValue == 1)
  }

  test("EXPIRE") {
    assert(expire(bs"toex", Int.MaxValue).futureValue)
  }

  test("EXPIREAT") {
    assert(expireat(bs"toex", Int.MaxValue).futureValue)
  }

  test("OBJECT REFCOUNT") {
    assert(objectRefcount(bs"???").futureValue.isEmpty)
    assert(objectRefcount(bs"key").futureValue.nonEmpty)
  }

  test("OBJECT ENCODING") {
    assert(objectEncoding(bs"???").futureValue.isEmpty)
    assert(objectEncoding(bs"key").futureValue.nonEmpty)
  }

  test("OBJECT IDLETIME") {
    assert(objectIdletime(bs"???").futureValue.isEmpty)
    assert(objectIdletime(bs"key").futureValue.nonEmpty)
  }

  test("PERSIST") {
    assert(!persist(bs"key").futureValue)
  }

  test("PEXPIRE") {
    assert(pexpire(bs"toex", Int.MaxValue).futureValue)
  }

  test("PEXPIREAT") {
    assert(pexpireat(bs"toex", Int.MaxValue).futureValue)
  }

  test("PTTL") {
    assert(pttl(bs"???").futureValue == Opt.Empty)
    assert(pttl(bs"key").futureValue == Opt(Opt.Empty))
    assert(pttl(bs"exkey").futureValue.exists(_.nonEmpty))
  }

  test("RENAME") {
    rename(bs"torename", keyWithSameSlotAs(bs"torename")).futureValue
  }

  test("RENAMENX") {
    assert(renamenx(bs"torenamenx", keyWithSameSlotAs(bs"torenamenx")).futureValue)
  }

  test("RESTORE") {
    val dumped = dump(bs"key").futureValue.get
    restore(bs"torestore", 1, dumped).futureValue
  }

  test("SORT") {
    assert(sort(bs"somelist",
      Opt(SelfPattern), Opt(SortLimit(0, 1)), asc = false, alpha = true).futureValue.isEmpty)
  }

  test("SORT with STORE") {
    assert(sortStore(bs"somelist", keyWithSameSlotAs(bs"somelist")).futureValue == 0)
  }

  test("TTL") {
    assert(ttl(bs"???").futureValue == Opt.Empty)
    assert(ttl(bs"key").futureValue == Opt(Opt.Empty))
    assert(ttl(bs"exkey").futureValue.exists(_.nonEmpty))
  }

  test("TYPE") {
    assert(`type`(bs"key").futureValue == RedisType.String)
  }
}

trait NodeKeysApiSuite extends ClusteredKeysApiSuite {
  type Api <: NodeKeysApi

  // only to make IntelliJ happy
  private lazy val cmds: NodeKeysApi {type Result[+A, -S] = Future[A]} = commands

  import cmds._

  private val scanKeys = (0 until 32).map(i => bs"toscan$i")

  override def setupCommands = {
    import RedisCommands._
    super.setupCommands *>
      scanKeys.map(set(_, bs"value")).sequence *>
      set(bs"todel2", bs"value") *>
      set(bs"tomove", bs"value")
  }

  test("KEYS") {
    assert(keys(bs"toscan*").futureValue.toSet == scanKeys.toSet)
  }

  test("SCAN") {
    def scanCollect(cursor: Cursor, acc: Seq[ByteString]): Future[Seq[ByteString]] =
      scan(cursor, Opt(bs"toscan*"), Opt(4L)).flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data)
        case (nextCursor, data) => scanCollect(nextCursor, acc ++ data)
      }
    assert(scanCollect(Cursor.NoCursor, Vector.empty).futureValue.toSet == scanKeys.toSet)
  }

  test("DEL multikey") {
    assert(del(Seq(bs"todel2", bs"foo")).futureValue == 1)
  }

  test("MOVE") {
    assert(move(bs"tomove", 1).futureValue)
  }

  test("EXISTS multikey") {
    assert(exists(Seq(bs"key", bs"foo")).futureValue == 1)
  }

  test("SORT with GET") {
    assert(sortGet(bs"somelist", Seq(HashFieldPattern(bs"hash", bs"*")),
      Opt(SelfPattern), Opt(SortLimit(0, 1)), asc = false, alpha = true).futureValue.isEmpty)
  }

  test("SORT with BY") {
    assert(sort(bs"somelist", by = SelfPattern.opt).futureValue.isEmpty)
    assert(sort(bs"somelist", by = KeyPattern(bs"sth_*").opt).futureValue.isEmpty)
    assert(sort(bs"somelist", by = HashFieldPattern(bs"hash_*", bs"sth_*").opt).futureValue.isEmpty)
  }
}

class RedisClusterKeysApiSuite extends RedisClusterCommandsSuite with ClusteredKeysApiSuite
class RedisNodeKeysApiSuite extends RedisNodeCommandsSuite with NodeKeysApiSuite
class RedisConnectionKeysApiSuite extends RedisConnectionCommandsSuite with NodeKeysApiSuite
