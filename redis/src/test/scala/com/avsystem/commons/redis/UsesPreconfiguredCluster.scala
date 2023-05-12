package com.avsystem.commons
package redis

import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest.Suite

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait UsesPreconfiguredCluster extends UsesActorSystem with UsesClusterServers { this: Suite =>

  def ports: Range = 9000 to 9005
  def preconfiguredDir: File = new File("preconfiguredCluster")

  protected def prepareDirectory(): Unit =
    FileUtils.copyDirectory(preconfiguredDir, clusterDir)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val clients = addresses.map(addr => new RedisConnectionClient(addr))
    val commands = clients.map(client => RedisApi.Connection.Async.BinaryTyped(client))
    val initFuture = Future.traverse(commands) { c =>
      waitUntil(
        for {
          stateOk <- c.clusterInfo.map(_.stateOk)
          slavesHaveSlots <- c.clusterSlots.map(_.forall(_.slaves.nonEmpty))
        } yield stateOk && slavesHaveSlots,
        500.millis,
      )
    }

    Await.result(initFuture, 30.seconds)

    clients.foreach(_.close())
  }
}
