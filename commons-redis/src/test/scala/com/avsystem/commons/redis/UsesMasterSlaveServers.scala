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
trait UsesMasterSlaveServers extends BeforeAndAfterAll with RedisProcessUtils { this: Suite =>

  val masterSlavePath: String = "masterSlave/" + System.currentTimeMillis()
  val masterSlaveDir: File = new File(masterSlavePath.replaceAllLiterally("/", File.separator))

  def masterName: String
  def ports: Seq[Int]
  def sentinelPorts: Seq[Int]

  lazy val addresses: Seq[NodeAddress] = ports.map(port => NodeAddress(port = port))
  lazy val sentinelAddresses: Seq[NodeAddress] = sentinelPorts.map(port => NodeAddress(port = port))

  var redisProcesses: Seq[RedisProcess] = _
  var sentinelProcesses: Seq[RedisProcess] = _

  protected def prepareDirectory(): Unit

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    prepareDirectory()
    val processesFut = Future.traverse(ports)(port => launchRedis(
      "--port", port.toString,
      "--daemonize", "no",
      "--pidfile", "redis.pid",
      "--dbfilename", "dump.rdb",
      "--dir", s"$masterSlavePath/$port"
    ))
    val sentinelsFut = Future.traverse(sentinelPorts)(port => launchSentinel(
      s"$masterSlavePath/$port/sentinel.conf",
      "--port", port.toString,
      "--daemonize", "no",
      "--pidfile", "redis.pid",
      "--dir", s"$masterSlavePath/$port"
    ))
    redisProcesses = Await.result(processesFut, 10.seconds)
    sentinelProcesses = Await.result(sentinelsFut, 10.seconds)
  }

  override protected def afterAll(): Unit = {
    Await.result(Future.traverse(redisProcesses ++ sentinelProcesses)(shutdownRedis), 10.seconds)
    FileUtils.deleteDirectory(masterSlaveDir)
    super.afterAll()
  }
}
