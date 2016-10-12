package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._

import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 03/10/16.
  */
trait ServerApiSuite extends CommandsSuite with UsesActorSystem {

  import RedisApi.Batches.StringTyped._

  apiTest("BGSAVE") {
    bgsave.get
    waitUntil(RedisApi.Batches.StringTyped.info(PersistenceInfo).map(_.rdbBgsaveInProgress.contains(false)).exec, 50.millis)
  }

  apiTest("BGREWRITEAOF") {
    bgrewriteaof.get
    waitUntil(RedisApi.Batches.StringTyped.info(PersistenceInfo).map(_.aofRewriteInProgress.contains(false)).exec, 50.millis)
  }

  apiTest("CLIENT LIST") {
    waitFor(clientList.exec)(_.nonEmpty, 100.millis).futureValue
  }

  apiTest("CLIENT PAUSE") {
    clientPause(10).get
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
    commandInfo("mget").assertEquals(Seq(CommandInfo("mget", CommandArity(2, more = true), CommandFlags.Readonly, 1, -1, 1)))
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

  apiTest("DEBUG OBJECT") {
    setup(set("key", "value"))
    debugObject("key").get
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
    role.assertEquals(MasterRole(0, Seq.empty))
  }

  apiTest("SAVE") {
    save.get
  }

  apiTest("SLAVEOF") {
    slaveofNoOne.get
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

trait NodeOnlyServerApiSuite extends ServerApiSuite {
  import RedisApi.Batches.StringTyped._

  apiTest("CLIENT KILL") {
    val clients: Seq[ClientInfo] = waitFor(clientList.exec)(_.size >= 3, 100.millis).futureValue
    clientKill(clients.head.addr).get
    clientKill(clients(1).addr, Skipme(false)).assertEquals(1)
    clientKill(clients(2).id, Skipme(false)).assertEquals(1)
    clientKill(ClientType.Master).assertEquals(0)
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

