package com.avsystem.commons
package redis

/**
  * Author: ghik
  * Created: 05/04/16.
  */
object Test {
  def main(args: Array[String]): Unit = {
    import java.util.concurrent.TimeUnit
    import java.util.concurrent.atomic.AtomicInteger

    import akka.actor.ActorSystem
    import akka.util.{ByteString, Timeout}
    import com.avsystem.commons.redis.config.{ClusterConfig, NodeConfig}

    import scala.concurrent.duration._
    import scala.concurrent.{Await, ExecutionContext, Future}
    import scala.util.{Failure, Success}
    import com.avsystem.commons.redis._

    implicit val as = ActorSystem()
    implicit val ec: ExecutionContext = as.dispatcher
    implicit val timeout = Timeout(60, TimeUnit.SECONDS)

    implicit class FutureGet[A](private val fut: Future[A]) {
      def get = Await.result(fut, timeout.duration)
    }

    implicit class StringOps(private val s: String) {
      def bytes = ByteString(s)
    }

    val client = new RedisClusterClient(List(NodeAddress(port = 33330)), ClusterConfig(nodeConfigs = _ => NodeConfig(poolSize = 1)))

    val ctr = new AtomicInteger(0)
    as.scheduler.schedule(Duration.Zero, 1.seconds) {
      val i = ctr.incrementAndGet()
      client.executeBatch(RedisStringCommands.get("costam"))
        .onComplete {
          case Success(result) => println(s"$i: SUCCESSFUL $result")
          case Failure(cause) => println(s"$i: FAILED $cause")
        }
    }
  }
}
