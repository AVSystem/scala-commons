package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.{RedisApi, RedisNodeCommandsSuite}

import scala.concurrent.duration._

class NodeOnlyServerApiSuite extends RedisNodeCommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("CLIENT KILL") {
    val clients: Seq[ClientInfo] = waitFor(clientList.exec)(_.size >= 3, 100.millis).futureValue
    clientKill(clients.head.addr).get
    clientKill(clients(1).addr, Skipme(false)).assertEquals(1)
    clientKill(clients(2).id, Skipme(false)).assertEquals(1)
    clientKill(ClientType.Master).assertEquals(0)
  }
}
