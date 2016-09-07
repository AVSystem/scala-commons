package com.avsystem.commons
package redis

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.avsystem.commons.redis.config.{ClusterConfig, NodeConfig}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Author: ghik
  * Created: 05/04/16.
  */
object Test {
  implicit val as = ActorSystem()
  implicit val ec: ExecutionContext = as.dispatcher
  implicit val timeout = Timeout(60, TimeUnit.SECONDS)

  implicit class FutureGet[A](private val fut: Future[A]) extends AnyVal {
    def get = Await.result(fut, timeout.duration)
  }

  implicit class StringOps(private val s: String) extends AnyVal {
    def bytes = ByteString(s)
  }

  def main(args: Array[String]): Unit = {
    val client = new RedisClusterClient(List(NodeAddress(port = 33330)), ClusterConfig(nodeConfigs = _ => NodeConfig(poolSize = 1)))

    val ctr = new AtomicInteger(0)
    as.scheduler.schedule(Duration.Zero, 1.seconds) {
      val i = ctr.incrementAndGet()
      client.executeBatch(RedisCommands.get("costam".bytes).map(_.map(_.utf8String)))
        .onComplete {
          case Success(result) => println(s"$i: SUCCESSFUL $result")
          case Failure(cause) => println(s"$i: FAILED $cause")
        }
    }
  }
}
