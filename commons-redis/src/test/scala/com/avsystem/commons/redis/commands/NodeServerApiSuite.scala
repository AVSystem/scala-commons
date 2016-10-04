package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._

import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 03/10/16.
  */
trait ServerApiSuite extends CommandsSuite { this: UsesActorSystem =>

  import RedisStringCommands._

  test("BGSAVE") {
    bgsave.get
  }

  test("CLIENT LIST") {
    waitFor(clientList.exec)(_.nonEmpty, 100.millis).futureValue
  }

  test("CLIENT PAUSE") {
    clientPause(10).get
  }

  test("COMMAND") {
    command.get
  }

  test("COMMAND COUNT") {
    commandCount.assert(_ > 150)
  }

  test("COMMAND GETKEYS") {
    commandGetkeys(RedisStringRawCommands.mset("key1" -> "value1", "key2" -> "value2", "key3" -> "value3"))
      .assertEquals(Seq("key1", "key2", "key3"))
  }

  test("COMMAND INFO") {
    commandInfo("mget").assertEquals(Seq(CommandInfo("mget", CommandArity(2, more = true), CommandFlags.Readonly, 1, -1, 1)))
  }

  test("CONFIG GET") {
    configGet("daemonize").assertEquals(Seq("daemonize" -> "no"))
  }

  test("CONFIG RESETSTAT") {
    configResetstat.get
  }

  test("CONFIG SET") {
    configSet("maxclients", "1000").get
  }

  test("DBSIZE") {
    dbsize.assertEquals(0)
  }

  test("DEBUG OBJECT") {
    setup(set("key", "value"))
    debugObject("key").get
  }

  test("FLUSHALL") {
    flushall.get
  }

  test("FLUSHDB") {
    flushdb.get
  }

  test("INFO") {
    RedisStringCommands.info.get
    RedisStringCommands.info(FullRedisInfo).get
    RedisStringCommands.info(CpuInfo).get
  }

  test("LASTSAVE") {
    lastsave.get
  }

  test("ROLE") {
    role.assertEquals(MasterRole(0, Seq.empty))
  }

  test("SAVE") {
    save.get
  }

  test("SLAVEOF") {
    slaveof(Opt.Empty).get
  }

  test("SLOWLOG GET") {
    slowlogGet.get
    slowlogGet(10).get
  }

  test("SLOWLOG LEN") {
    slowlogLen.get
  }

  test("SLOWLOG RESET") {
    slowlogReset.get
  }

  test("TIME") {
    time.get
  }
}

trait NodeServerApiSuite extends ServerApiSuite { this: UsesActorSystem =>
  import RedisStringCommands._

  test("CLIENT KILL") {
    val clients: Seq[ClientInfo] = waitFor(clientList.exec)(_.size >= 3, 100.millis).futureValue
    clients.foreach(println)
    clientKill(clients.head.addr).get
    clientKill(clients(1).addr, Skipme(false)).assertEquals(1)
    clientKill(clients(2).id, Skipme(false)).assertEquals(1)
    clientKill(ClientType.Master).assertEquals(0)
  }
}

trait ConnectionServerApiSuite extends ServerApiSuite { this: UsesActorSystem =>
  import RedisStringCommands._

  test("CLIENT GETNAME") {
    clientGetname.assertEquals(Opt.Empty)
    setup(clientSetname("name"))
    clientGetname.assertEquals("name".opt)
  }

  test("CLIENT SETNAME") {
    clientSetname("name").get
  }
}

class RedisNodeServerApiSuite extends RedisNodeCommandsSuite with NodeServerApiSuite
class RedisConnectionServerApiSuite extends RedisConnectionCommandsSuite with ConnectionServerApiSuite
