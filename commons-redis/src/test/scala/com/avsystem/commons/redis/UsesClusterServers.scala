package com.avsystem.commons
package redis

import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait UsesClusterServers extends BeforeAndAfterAll with RedisProcessUtils { this: Suite =>

  val clusterPath: String = "cluster/" + System.currentTimeMillis()
  val clusterDir: File = new File(clusterPath.replaceAllLiterally("/", File.separator))

  def ports: Seq[Int]

  lazy val addresses: Seq[NodeAddress] = ports.map(port => NodeAddress(port = port))
  var redisProcesses: Seq[RedisProcess] = _

  protected def prepareDirectory(): Unit

  protected def slotKey(slot: Int): String = ClusterUtils.SlotKeys(slot)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    prepareDirectory()
    redisProcesses = Await.result(Future.traverse(ports)(port => launchRedis(
      "--port", port.toString,
      "--daemonize", "no",
      "--pidfile", "redis.pid",
      "--dbfilename", "dump.rdb",
      "--dir", s"$clusterPath/$port",
      "--appendonly", "yes",
      "--appendfilename", "appendonly.aof",
      "--cluster-enabled", "yes",
      "--cluster-config-file", "nodes.conf"
    )), 10.seconds)
  }

  override protected def afterAll(): Unit = {
    Await.result(Future.traverse(ports zip redisProcesses) {
      case (port, process) => shutdownRedis(port, process)
    }, 10.seconds)
    FileUtils.deleteDirectory(clusterDir)
    super.afterAll()
  }
}
