package com.avsystem.commons
package redis

import java.io.File

import org.scalatest.Suite

import scala.concurrent.Await
import scala.concurrent.duration._

/** Author: ghik Created: 27/06/16.
  */
trait UsesFreshCluster extends UsesActorSystem with UsesClusterServers { this: Suite =>

  def ports: Range = 8000 to 8005
  def masterOf(idx: Int): Int = idx - (idx % 2)

  lazy val masterIndices: IIndexedSeq[Int] = ports.indices.filter(i => masterOf(i) == i)
  lazy val slotRanges: IIndexedSeq[Range] = ports.indices.map { i =>
    val masterNo = masterIndices.indexOf(masterOf(i))
    firstSlot(masterNo) until firstSlot(masterNo + 1)
  }

  protected def prepareDirectory(): Unit = {
    ports.foreach { port =>
      new File(clusterDir, port.toString).mkdirs()
    }
  }

  def firstSlot(masterNo: Int): Int = masterNo * Hash.TotalSlots / masterIndices.size

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val clients = addresses.map(addr => new RedisConnectionClient(addr))
    val commands = clients.map(client => RedisApi.Connection.Async.BinaryTyped(client))

    val initFuture = for {
      _ <- Future.traverse(commands.tail)(_.clusterMeet(addresses.head))
      _ <- Future.traverse(commands)(c => waitUntil(c.clusterInfo.map(_.knownNodes >= addresses.size), 500.millis))
      nodeIds <- commands.head.clusterNodes.map(_.sortBy(_.address.port).map(_.id))
      _ <- Future.traverse(commands.zipWithIndex) { case (c, i) =>
        masterOf(i) match {
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
