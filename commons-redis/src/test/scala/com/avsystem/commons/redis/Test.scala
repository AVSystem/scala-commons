package com.avsystem.commons
package redis

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.avsystem.commons.redis.commands.ShutdownModifier

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Author: ghik
  * Created: 05/04/16.
  */
object Test {
  def main(args: Array[String]): Unit = {
    implicit val as = ActorSystem()
    implicit val ec: ExecutionContext = as.dispatcher
    implicit val timeout = Timeout(60, TimeUnit.SECONDS)

    implicit class FutureGet[A](private val fut: Future[A]) {
      def get = Await.result(fut, timeout.duration)
    }

    implicit class StringOps(private val s: String) {
      def bytes = ByteString(s)
    }

    val shutdownClient = new RedisConnectionClient(NodeAddress(port = 33332))
    RedisApi.Connection.Async.StringTyped(shutdownClient).shutdown(ShutdownModifier.Nosave)

    Thread.sleep(5)
    println("SHUTDOWN COMPLETE")

    val client = new RedisClusterClient(List(NodeAddress(port = 33332), NodeAddress(port = 33333)))
    Await.result(client.initialized, Duration.Inf)
  }
}
