package com.avsystem.commons
package redis

import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._
import scala.concurrent.Await

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait UsesClusterServers extends BeforeAndAfterAll with RedisProcessUtils { this: Suite =>

  val clusterDir = new File("cluster/" + System.currentTimeMillis())

  def ports: Seq[Int]

  lazy val addresses = ports.map(port => NodeAddress(port = port))
  var redisProcesses: Seq[RedisProcess] = _

  protected def prepareDirectory(): Unit

  protected def slotKey(slot: Int) = ClusterUtils.SlotKeys(slot)

  override protected def beforeAll() = {
    super.beforeAll()
    prepareDirectory()
    redisProcesses = Await.result(Future.traverse(ports)(port => launchRedis(
      "--port", port.toString,
      "--daemonize", "no",
      "--pidfile", "redis.pid",
      "--dbfilename", "dump.rdb",
      "--dir", s"$clusterDir/$port",
      "--appendonly", "yes",
      "--appendfilename", "appendonly.aof",
      "--cluster-enabled", "yes",
      "--cluster-config-file", "nodes.conf"
    )), 10.seconds)
  }

  override protected def afterAll() = {
    Await.result(Future.traverse(ports zip redisProcesses) {
      case (port, process) => shutdownRedis(port, process)
    }, 10.seconds)
    FileUtils.deleteDirectory(clusterDir)
    super.afterAll()
  }
}
