package com.avsystem.commons
package redis

import com.avsystem.commons.redis.commands.ShutdownModifier
import com.avsystem.commons.redis.config.{ClusterConfig, ConnectionConfig, ExecutionConfig, NodeConfig}
import com.avsystem.commons.redis.exception.{ClusterInitializationException, CrossSlotException, ForbiddenCommandException, NoKeysException}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Await
import scala.concurrent.duration._

class RedisClusterClientNonClusteredInitTest extends AnyFunSuite
  with Matchers with ScalaFutures with UsesActorSystem with UsesRedisServer {

  import RedisApi.Batches.StringTyped._

  override def password: Opt[String] = "pass".opt

  def createClient(port: Int, pass: String, fallbackToSingleNode: Boolean = false): RedisClusterClient = {
    val connConfig = ConnectionConfig(initCommands = auth(pass))
    val nodeConfig = NodeConfig(connectionConfigs = _ => connConfig)
    val config = ClusterConfig(
      nodeConfigs = _ => nodeConfig,
      monitoringConnectionConfigs = _ => connConfig,
      fallbackToSingleNode = fallbackToSingleNode
    )
    new RedisClusterClient(List(NodeAddress(port = port)), config)
  }

  test("seed connection failure test") {
    val client = createClient(63789, "")
    client.initialized.failed.futureValue shouldBe a[ClusterInitializationException]
    client.executeBatch(get("lol")).failed.futureValue shouldBe a[ClusterInitializationException]
  }

  test("seed connection init failure test") {
    val client = createClient(port, "badpass")
    client.initialized.failed.futureValue shouldBe a[ClusterInitializationException]
    client.executeBatch(get("lol")).failed.futureValue shouldBe a[ClusterInitializationException]
  }

  test("cluster state fetching failure test") {
    val client = createClient(port, "pass")
    client.initialized.failed.futureValue shouldBe a[ClusterInitializationException]
    client.executeBatch(get("lol")).failed.futureValue shouldBe a[ClusterInitializationException]
  }

  test("fallback to single node test") {
    val client = createClient(port, "pass", fallbackToSingleNode = true)
    client.initialized.futureValue shouldBe client
    client.executeBatch(get("lol")).futureValue shouldBe Opt.Empty
  }
}

class RedisClusterClientInitTest extends AnyFunSuite
  with Matchers with ScalaFutures with UsesActorSystem with UsesPreconfiguredCluster {

  import RedisApi.Batches.StringTyped._

  def createClient(ports: Int*): RedisClusterClient =
    new RedisClusterClient(ports.map(p => NodeAddress(port = p)))

  test("client init test") {
    val client = createClient(ports.head)
    client.initialized.futureValue shouldBe client
    client.executeBatch(get(slotKey(0))).futureValue shouldBe Opt.Empty
  }

  test("client init test with one seed down") {
    val client = createClient(ports.head, ports.head - 1)
    client.initialized.futureValue shouldBe client
    client.executeBatch(get(slotKey(0))).futureValue shouldBe Opt.Empty
  }
}

class RedisClusterClientInitDuringFailureTest extends AnyFunSuite
  with Matchers with ScalaFutures with UsesActorSystem with UsesPreconfiguredCluster {

  import RedisApi.Batches.StringTyped._

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(Span(120, Seconds), Span(1, Seconds))

  def createClient(ports: Int*) = new RedisClusterClient(ports.map(p => NodeAddress(port = p)))

  test("client init during failure test") {
    new RedisConnectionClient(NodeAddress(port = 9000))
      .executeBatch(shutdown(ShutdownModifier.Nosave))

    val client = createClient(ports.head, ports.head + 1)
    client.initialized.futureValue shouldBe client
    client.executeBatch(get(slotKey(0)), ExecutionConfig(responseTimeout = 120.seconds)).futureValue shouldBe Opt.Empty
  }
}

class RedisClusterClientTest extends RedisClusterCommandsSuite {

  import RedisApi.Batches.StringTyped._

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
    val batch = mget(Seq(0, 7000).map(slotKey))
    batch.intercept[CrossSlotException]
  }

  test("cross slot on multikey transaction") {
    val batch = Seq(0, 7000).map(i => get(slotKey(i))).sequence.transaction
    batch.intercept[CrossSlotException]
  }

  test("forbidden command") {
    val batch = watch(slotKey(0))
    batch.intercept[ForbiddenCommandException]
  }
}

class ClusterSlotMigrationTest extends RedisClusterCommandsSuite {

  import RedisApi.Batches.StringTyped._

  test("empty slot migration") {
    migrateSlot(0, 7000).futureValue
  }

  test("single key slot migration") {
    setup(set(slotKey(1), "value"))
    migrateSlot(1, 7000).futureValue
  }

  test("multiple keys slot migration") {
    setup(mset((0 until 10).map(i => (s"{${slotKey(2)}}$i", "value"))))
    migrateSlot(2, 7000).futureValue
  }
}

class ClusterRedirectionHandlingTest extends RedisClusterCommandsSuite {

  import RedisApi.Batches.StringTyped._

  // don't refresh cluster state
  override def clusterConfig: ClusterConfig =
    super.clusterConfig.copy(minRefreshInterval = Int.MaxValue.seconds)

  override protected def beforeAll(): Unit = {
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

  test("TRYAGAIN after redirection") {
    val k1 = s"{${slotKey(1)}}1"
    val k2 = s"{${slotKey(1)}}2"
    val mgetFut = mget(k1, k2).exec
    val txFut = (get(k1), get(k2)).sequence.transaction.exec
    Thread.sleep(500)
    (set(k1, "v1"), set(k2, "v2")).sequence.get
    assert(mgetFut.futureValue == Seq("v1".opt, "v2".opt))
    assert(txFut.futureValue == ("v1".opt, "v2".opt))
    assert(listener.result().contains("-TRYAGAIN"))
  }

  test("TRYAGAIN before redirection") {
    Await.result(set(s"{${slotKey(2)}}1", "v1").exec, Duration.Inf)
    Await.result(migrateSlot(2, 7000, incomplete = true, withoutData = true), Duration.Inf)

    val k1 = s"{${slotKey(2)}}1"
    val k2 = s"{${slotKey(2)}}2"
    val mgetFut = mget(k1, k2).exec
    val txFut = (get(k1), get(k2)).sequence.transaction.exec
    Thread.sleep(500)
    val targetAddress = redisClient.currentState.clientForSlot(7000).address
    (migrate(Seq(k1), targetAddress, 0, 1000), set(k2, "v2")).sequence.assertEquals((true, true))
    assert(mgetFut.futureValue == Seq("v1".opt, "v2".opt))
    assert(txFut.futureValue == ("v1".opt, "v2".opt))
    assert(listener.result().contains("-TRYAGAIN"))
  }
}

class ClusterFailoverHandlingTest extends RedisClusterCommandsSuite {

  import RedisApi.Batches.StringTyped._

  // don't refresh cluster state
  override def clusterConfig: ClusterConfig = super.clusterConfig.copy(minRefreshInterval = Int.MaxValue.seconds)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val slaveClient = new RedisConnectionClient(NodeAddress(port = 9001))
    def failover(delay: FiniteDuration = Duration.Zero): Future[Unit] = for {
      master <- slaveClient.executeBatch(clusterNodes.map(_.find(_.flags.myself).exists(_.flags.master)))
      _ <- {
        if (master) Future.successful(())
        else for {
          _ <- wait(delay)
          _ <- slaveClient.executeBatch(clusterFailover.ignoreFailures)
          _ <- failover(1.seconds)
        } yield ()
      }
    } yield ()
    Await.result(redisClient.initialized.flatMapNow(_ => failover()), Duration.Inf)
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
