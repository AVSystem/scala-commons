package com.avsystem.commons
package redis

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * Author: ghik
  * Created: 08/09/16.
  */
object Profiled {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("redis", ConfigFactory.defaultReference.withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("INFO")))
    val client = new RedisClusterClient(List(NodeAddress(port = 33330)))
    while (true) {
      def singleFut = client.executeBatch((0 until 1000)
        .map(i => RedisCommands.get(ByteString(s"costam$i"))).sequence)(Timeout(5, TimeUnit.SECONDS))
      import com.avsystem.commons.concurrent.RunInQueueEC.Implicits.executionContext
      val resultFut = Future.traverse(0 until 100: IndexedSeq[Int])(_ => singleFut)
      Await.result(resultFut, Duration.Inf)
    }
  }
}
