package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisFlushable

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait KeysApiSuite extends CommandsSuite {

  import commands._

  override def setupCommands = super.setupCommands *>
    RedisCommands.set(bs"key", bs"value") *>
    RedisCommands.setex(bs"exkey", Int.MaxValue, bs"value") *>
    RedisCommands.set(bs"toex", bs"value") *>
    RedisCommands.set(bs"todel", bs"value") *>
    RedisCommands.set(bs"torename", bs"value") *>
    RedisCommands.set(bs"torenamenx", bs"value") *>
    RedisCommands.set(bs"tomove", bs"value") *>
    RedisFlushable.success(())

  test("DEL") {
    assert(del(Seq(bs"todel", bs"foo")).futureValue == 1)
  }

  test("DUMP") {
    assert(dump(bs"???").futureValue.isEmpty)
    assert(dump(bs"key").futureValue.nonEmpty)
  }

  test("EXISTS") {
    assert(exists(Seq(bs"key", bs"foo")).futureValue == 1)
  }

  test("EXPIRE") {
    assert(expire(bs"toex", Int.MaxValue).futureValue)
  }

  test("EXPIREAT") {
    assert(expireat(bs"toex", Int.MaxValue).futureValue)
  }

  test("MOVE") {
    assert(move(bs"tomove", 1).futureValue)
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
    rename(bs"torename", bs"renamed").futureValue
  }

  test("RENAMENX") {
    assert(renamenx(bs"torenamenx", bs"renamednx").futureValue)
  }

  test("RESTORE") {
    val dumped = dump(bs"key").futureValue.get
    restore(bs"torestore", 1, dumped).futureValue
  }

  test("SORT") {
    assert(sort(bs"somelist",
      Opt(SelfPattern), Opt(SortLimit(0, 1)), asc = false, alpha = true).futureValue.isEmpty)
    assert(sortGet(bs"somelist", Seq(HashFieldPattern(bs"hash", bs"*")),
      Opt(SelfPattern), Opt(SortLimit(0, 1)), asc = false, alpha = true).futureValue.isEmpty)
    assert(sortStore(bs"somelist", bs"destination").futureValue == 0)
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

class RedisNodeKeysApiSuite extends RedisNodeCommandsSuite with KeysApiSuite
