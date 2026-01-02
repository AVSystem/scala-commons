package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._
import com.avsystem.commons.redis.exception.ErrorReplyException

import scala.concurrent.duration._

/** Author: ghik Created: 03/10/16.
  */
trait ServerApiSuite extends CommandsSuite with UsesActorSystem {

  import RedisApi.Batches.StringTyped._

  def waitForPersistence() =
    waitUntil(
      RedisApi.Batches.StringTyped
        .info(PersistenceInfo)
        .map { pi =>
          pi.rdbBgsaveInProgress.contains(false) && pi.aofRewriteInProgress.contains(false)
        }
        .exec,
      50.millis,
    )

  apiTest("BGSAVE") {
    waitForPersistence()
    bgsave.get
  }

  apiTest("BGREWRITEAOF") {
    waitForPersistence()
    bgrewriteaof.get
  }

  apiTest("CLIENT ID") {
    clientId.get
  }

  apiTest("CLIENT LIST") {
    waitFor(clientList.exec)(_.nonEmpty, 100.millis).futureValue
  }

  apiTest("CLIENT PAUSE") {
    clientPause(10).get
  }

  apiTest("CLIENT UNBLOCK") {
    clientUnblock(ClientId(0)).assertEquals(false)
    clientUnblock(ClientId(0), UnblockModifier.Timeout).assertEquals(false)
    clientUnblock(ClientId(0), UnblockModifier.Error).assertEquals(false)
  }

  apiTest("COMMAND") {
    command.get
  }

  apiTest("COMMAND COUNT") {
    commandCount.assert(_ > 150)
  }

  apiTest("COMMAND GETKEYS") {
    commandGetkeys(RedisApi.Raw.StringTyped.mset("key1" -> "value1", "key2" -> "value2", "key3" -> "value3"))
      .assertEquals(Seq("key1", "key2", "key3"))
  }

  apiTest("COMMAND INFO") {
    commandInfo("mget").assertEquals(
      CommandInfo("mget", CommandArity(2, more = true), CommandFlags.Readonly | CommandFlags.Fast, 1, -1, 1)
    )
  }

  apiTest("CONFIG GET") {
    configGet("daemonize").assertEquals(Seq("daemonize" -> "no"))
  }

  apiTest("CONFIG RESETSTAT") {
    configResetstat.get
  }

  apiTest("CONFIG SET") {
    configSet("maxclients", "1000").get
  }

  apiTest("DBSIZE") {
    dbsize.assertEquals(0)
  }

  apiTest("FLUSHALL") {
    flushall.get
  }

  apiTest("FLUSHDB") {
    flushdb.get
  }

  apiTest("INFO") {
    RedisApi.Batches.StringTyped.info.get
    RedisApi.Batches.StringTyped.info(FullRedisInfo).get
    RedisApi.Batches.StringTyped.info(CpuInfo).get
  }

  apiTest("LASTSAVE") {
    lastsave.get
  }

  apiTest("ROLE") {
    role.get
  }

  apiTest("SAVE") {
    waitForPersistence()
    try save.get
    catch {
      // ignore spurious Redis failures
      case e: ErrorReplyException if e.errorStr == "ERR Background save already in progress" =>
    }
  }

  apiTest("SLAVEOF") {
    slaveofNoOne.get
  }

  apiTest("REPLICAOF") {
    replicaofNoOne.get
  }

  apiTest("SLOWLOG GET") {
    slowlogGet.get
    slowlogGet(10).get
  }

  apiTest("SLOWLOG LEN") {
    slowlogLen.get
  }

  apiTest("SLOWLOG RESET") {
    slowlogReset.get
  }

  apiTest("TIME") {
    time.get
  }
}

trait ConnectionServerApiSuite extends ServerApiSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("CLIENT GETNAME") {
    clientGetname.assertEquals(Opt.Empty)
    setup(clientSetname("name"))
    clientGetname.assertEquals("name".opt)
  }

  apiTest("CLIENT SETNAME") {
    clientSetname("name").get
  }
}
