package com.avsystem.commons
package redis

import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest.Suite

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait UsesPreconfiguredCluster extends UsesActorSystem with UsesClusterServers {this: Suite =>

  final def ports = 9000 to 9005

  protected def prepareDirectory() = {
    FileUtils.copyDirectory(new File("preconfiguredCluster"), clusterDir)
  }

  override protected def beforeAll() = {
    super.beforeAll()

    val clients = addresses.map(addr => new RedisConnectionClient(addr))
    val commands = clients.map(client => new RedisBinaryConnectionAsyncCommands(client.toExecutor))
    val initFuture = Future.traverse(commands)(c => waitUntil(c.clusterInfo.map(_.stateOk), 500.millis))

    Await.result(initFuture, 30.seconds)

    clients.foreach(_.close())
  }
}
