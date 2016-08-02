package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.config.{ClusterConfig, NodeConfig}
import com.avsystem.commons.redis.exception.{CrossSlotException, ForbiddenCommandException, NoKeysException}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
class RedisClusterClientTest extends RedisClusterCommandsSuite {

  import RedisCommands._

  override def clusterConfig = ClusterConfig(nodeConfigs = addr => NodeConfig(poolSize = 1))

  test("simple get") {
    get(bs"key").assertEquals(Opt.Empty)
  }

  test("distribution") {
    val slots = List(
      0,
      7000,
      7001,
      1,
      14000,
      14001,
      7002
    )
    setup(slots.map(s => set(ClusterUtils.SlotKeys(s), bs"$s")).sequence)
    val batch = slots.map(s => get(ClusterUtils.SlotKeys(s))).sequence
    batch.assertEquals(slots.map(s => bs"$s".opt))
  }

  test("no keys") {
    flushall.intercept[NoKeysException]
  }

  test("cross slot on multikey command") {
    val batch = mget(Seq(0, 7000).map(ClusterUtils.SlotKeys))
    batch.intercept[CrossSlotException]
  }

  test("cross slot on multikey transaction") {
    val batch = Seq(0, 7000).map(i => get(ClusterUtils.SlotKeys(i))).sequence.transaction
    batch.intercept[CrossSlotException]
  }

  test("forbidden command") {
    val batch = watch(Seq(ClusterUtils.SlotKeys(0)))
    batch.intercept[ForbiddenCommandException]
  }
}
