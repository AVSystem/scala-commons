package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.exception.{CrossSlotException, ForbiddenCommandException, NoKeysException}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
class RedisClusterClientTest extends RedisClusterCommandsSuite {

  import RedisStringCommands._

  test("simple get") {
    get("key").assertEquals(Opt.Empty)
  }

  test("distribution") {
    val slots = List(0, 7000, 7001, 1, 14000, 14001, 7002)
    setup(slots.map(s => set(slotKey(s), s"$s")).sequence)
    val batch = slots.map(s => get(slotKey(s))).sequence
    batch.assertEquals(slots.map(s => s"$s".opt))
  }

  test("no keys") {
    flushall.intercept[NoKeysException]
  }

  test("cross slot on multikey command") {
    val batch = mget(Seq(0, 7000).map(slotKey): _*)
    batch.intercept[CrossSlotException]
  }

  test("cross slot on multikey transaction") {
    val batch = Seq(0, 7000).map(i => get(slotKey(i))).sequence.transaction
    batch.intercept[CrossSlotException]
  }

  test("forbidden command") {
    val batch = watch(Seq(slotKey(0)))
    batch.intercept[ForbiddenCommandException]
  }
}

class ClusterSlotMigrationTest extends RedisClusterCommandsSuite {

  import RedisStringCommands._

  test("empty slot migration") {
    migrateSlot(0, 7000).futureValue
  }

  test("single key slot migration") {
    setup(set(slotKey(1), "value"))
    migrateSlot(1, 7000).futureValue
  }

  test("multiple keys slot migration") {
    setup(mset((0 until 10).map(i => (s"{${slotKey(2)}}$i", "value")): _*))
    migrateSlot(2, 7000).futureValue
  }
}

class ClusterRedirectionHandlingTest extends RedisClusterCommandsSuite {

  import RedisStringCommands._

  // don't refresh cluster state
  override def clusterConfig = super.clusterConfig.copy(minRefreshInterval = Int.MaxValue.seconds)

  override protected def beforeAll() = {
    super.beforeAll()
    Await.result(migrateSlot(0, 7000), Duration.Inf)
    Await.result(migrateSlot(1, 7000, incomplete = true), Duration.Inf)
  }

  test("redirection handling after migration") {
    get(slotKey(0)).assertEquals(Opt.Empty)
    assert(listener.result().contains("-MOVED 0 127.0.0.1:9002"))
  }

  test("transaction redirection handling after migration") {
    (get(slotKey(0)) <* randomkey).transaction.assertEquals(Opt.Empty)
    assert(listener.result().contains("-MOVED 0 127.0.0.1:9002"))
  }

  test("redirection handling during migration") {
    get(slotKey(1)).assertEquals(Opt.Empty)
    assert(listener.result().contains("-ASK 1 127.0.0.1:9002"))
  }

  test("transaction redirection handling during migration") {
    (get(slotKey(1)) <* randomkey).transaction.assertEquals(Opt.Empty)
    assert(listener.result().contains("-ASK 1 127.0.0.1:9002"))
  }

  test("composite batch redirection handling after migration") {
    val slots = List(0, 7000, 7001, 1, 14000, 14001, 7002, 0, 1, 2, 3)
    val batch = slots.map(s => get(slotKey(s))).sequence
    batch.assertEquals(slots.map(_ => Opt.Empty))
  }
}

class ClusterFailoverHandlingTest extends RedisClusterCommandsSuite {

  import RedisStringCommands._

  // don't refresh cluster state
  override def clusterConfig = super.clusterConfig.copy(minRefreshInterval = Int.MaxValue.seconds)

  override protected def beforeAll() = {
    super.beforeAll()
    val slaveClient = new RedisConnectionClient(NodeAddress(port = 9001))
    def failover: Future[Unit] = for {
      master <- slaveClient.execute(clusterNodes.map(_.find(_.flags.myself).exists(_.flags.master)))
      _ <- {
        if (master) Future.successful(())
        else for {
          _ <- slaveClient.execute(clusterFailover().ignoreFailures)
          _ <- wait(1.seconds)
          _ <- failover
        } yield ()
      }
    } yield ()
    Await.result(redisClient.initialized.flatMapNow(_ => failover), Duration.Inf)
  }

  test("redirection caused by failover handling") {
    get(slotKey(0)).assertEquals(Opt.Empty)
    assert(listener.result().contains("-MOVED 0 127.0.0.1:9001"))
  }

  test("transaction redirection caused by failover handling") {
    (get(slotKey(0)) <* randomkey).transaction.assertEquals(Opt.Empty)
    assert(listener.result().contains("-MOVED 0 127.0.0.1:9001"))
  }
}
