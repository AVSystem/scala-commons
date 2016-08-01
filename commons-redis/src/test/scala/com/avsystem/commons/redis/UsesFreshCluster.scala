package com.avsystem.commons
package redis

import java.io.File

import org.scalatest.Suite

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait UsesFreshCluster extends UsesActorSystem with UsesClusterServers {this: Suite =>

  def ports = 8000 to 8005
  def masterOf(idx: Int): Int = idx - (idx % 2)

  lazy val masterIndices = ports.indices.filter(i => masterOf(i) == i)
  lazy val slotRanges = ports.indices.map { i =>
    val masterNo = masterIndices.indexOf(masterOf(i))
    firstSlot(masterNo) until firstSlot(masterNo + 1)
  }

  protected def prepareDirectory() = {
    ports.foreach { port =>
      new File(clusterDir, port.toString).mkdirs()
    }
  }

  def firstSlot(masterNo: Int) = masterNo * Hash.TotalSlots / masterIndices.size

  def wait(duration: FiniteDuration): Future[Unit] = {
    val promise = Promise[Unit]()
    actorSystem.scheduler.scheduleOnce(duration)(promise.success(()))
    promise.future
  }

  def waitUntil(predicate: => Future[Boolean], retryInterval: FiniteDuration): Future[Unit] =
    predicate.flatMap { r =>
      if (r) Future.successful(())
      else wait(retryInterval).flatMap(_ => waitUntil(predicate, retryInterval))
    }

  override protected def beforeAll() = {
    super.beforeAll()

    val clients = addresses.map(addr => new RedisConnectionClient(addr))
    val commands = clients.map(client => RedisConnectionAsyncCommands(client.toExecutor))

    val initFuture = for {
      _ <- Future.traverse(commands.tail)(_.clusterMeet(addresses.head))
      _ <- Future.traverse(commands)(c => waitUntil(c.clusterInfo.map(_.knownNodes >= addresses.size), 500.millis))
      nodeIds <- commands.head.clusterNodes.map(_.sortBy(_.address.port).map(_.id))
      _ <- Future.traverse(commands.zipWithIndex) {
        case (c, i) => masterOf(i) match {
          case `i` => c.clusterAddslots(slotRanges(i))
          case mi => c.clusterReplicate(nodeIds(mi))
        }
      }
      _ <- Future.traverse(commands)(c => waitUntil(c.clusterInfo.map(_.stateOk), 500.millis))
    } yield ()

    Await.result(initFuture, 30.seconds)

    clients.foreach(_.close())
  }
}
