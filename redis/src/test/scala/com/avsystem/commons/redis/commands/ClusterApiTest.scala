package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.{ClusterUtils, NodeAddress, RedisApi, RedisClusterCommandsSuite, RedisNodeClient}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class ClusterApiTest extends RedisClusterCommandsSuite {
  override def executor: RedisNodeClient = Await.result(redisClient.initializedCurrentState, Duration.Inf).mapping.head._2

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

  test("CLUSTER REPLICAS") {
    clusterReplicas(NodeId("b714a8032b9c1d74a7adc7da75fdbde0517bdf1b"))
      .map(_.map(_.id)).assertEquals(Seq(NodeId("cc8228f6e849ba1ee5abfc8f1ebde238e08c1d27")))
  }

  test("CLUSTER SLOTS") {
    clusterSlots.map(_.sortBy(_.master.port)).assertEquals(Seq(
      SlotRangeMapping(SlotRange(0, 5460), NodeAddress(port = 9000), NodeId("b714a8032b9c1d74a7adc7da75fdbde0517bdf1b").opt,
        Seq((NodeAddress(port = 9001), NodeId("cc8228f6e849ba1ee5abfc8f1ebde238e08c1d27").opt))),
      SlotRangeMapping(SlotRange(5461, 10921), NodeAddress(port = 9002), NodeId("5f4c1e93370f3a60e9106cff3a613216abb1c8dc").opt,
        Seq((NodeAddress(port = 9003), NodeId("18346204561dbae251912d8ae93fa4c78eeb3e16").opt))),
      SlotRangeMapping(SlotRange(10922, 16383), NodeAddress(port = 9004), NodeId("6a724c321662027e9c1c58684ea82a1315a294fb").opt,
        Seq((NodeAddress(port = 9005), NodeId("9a90efc8f9cf52de6aa60be3da3071798e0e365f").opt)))
    ))
  }

  test("CLUSTER SLAVES") {
    val id = clusterMyid.get
    clusterSlaves(id).assert(_.size == 1)
  }
}
