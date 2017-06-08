package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.{ClusterUtils, NodeAddress, RedisApi, RedisClusterCommandsSuite}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class ClusterApiTest extends RedisClusterCommandsSuite {
  override def executor = Await.result(redisClient.initializedCurrentState, Duration.Inf).mapping.head._2

  import RedisApi.Batches.StringTyped._

  test("CLUSTER DELSLOTS/ADDSLOTS") {
    clusterDelslots(0 to 16).get
    clusterAddslots(0 to 16).get
    clusterDelslots(5, 8, 10).get
    clusterAddslots(5, 8, 10).get
  }

  test("CLUSTER COUNT-FAILURE-REPORTS") {
    val id = clusterMyid.get
    clusterCountFailureReports(id).assert(_ >= 0)
  }

  test("CLUSTER COUNTKEYSINSLOT") {
    val tag = ClusterUtils.SlotKeys(0)
    setup(mset((0 until 10).map(i => (s"{$tag}$i", ""))))
    clusterCountkeysinslot(0).assertEquals(10)
    clusterCountkeysinslot(1).assertEquals(0)
  }

  test("CLUSTER GETKEYSINSLOT") {
    val tag = ClusterUtils.SlotKeys(0)
    setup(mset((0 until 10).map(i => (s"{$tag}$i", ""))))
    clusterGetkeysinslot(0, 10).map(_.sorted).assertEquals((0 until 10).map(i => s"{$tag}$i"))
  }

  test("CLUSTER INFO") {
    clusterInfo.assert(_.stateOk)
  }

  test("CLUSTER MYID") {
    clusterMyid.get
  }

  test("CLUSTER NODES") {
    clusterNodes.assert(_.size == ports.size)
  }

  test("CLUSTER SLOTS") {
    clusterSlots.map(_.sortBy(_.master.port)).assertEquals(Seq(
      SlotRangeMapping(SlotRange(0, 5460), NodeAddress(port = 9000), Seq(NodeAddress(port = 9001))),
      SlotRangeMapping(SlotRange(5461, 10921), NodeAddress(port = 9002), Seq(NodeAddress(port = 9003))),
      SlotRangeMapping(SlotRange(10922, 16383), NodeAddress(port = 9004), Seq(NodeAddress(port = 9005)))
    ))
  }

  test("CLUSTER SLAVES") {
    val id = clusterMyid.get
    clusterSlaves(id).assert(_.size == 1)
  }
}
