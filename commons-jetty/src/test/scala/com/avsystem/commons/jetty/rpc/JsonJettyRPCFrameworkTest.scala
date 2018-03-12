package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rpc.RPC
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.server.Server

/**
  * @author MKej
  */
object JsonJettyRPCFrameworkTest {
  import JsonJettyRPCFramework._

  @RPC trait SomeApi {
    def keks: Future[Int]
    def isTop(keks: Int): Future[Boolean]
    def topper: Topper
    def erroneousKeks: Future[Int]
  }
  object SomeApi {
    implicit val fullRPCInfo: BaseFullRPCInfo[SomeApi] = materializeFullInfo
  }

  @RPC trait Topper {
    def initialize: Future[Unit]
    def topKeks: Future[Int]
  }
  object Topper {
    implicit val fullRPCInfo: BaseFullRPCInfo[Topper] = materializeFullInfo
  }

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val impl = new SomeApi {
      def keks = Future.successful(131)
      def isTop(keks: Int) = Future.successful(keks == Int.MaxValue)
      object topper extends Topper {
        def initialize = Future.successful(println("Topper initialized"))
        def topKeks = Future.successful(Int.MaxValue)
      }
      def erroneousKeks: Future[Int] = Future.failed(new RuntimeException("cannot into"))
    }

    val port = 1337

    val server = new Server(port)
    server.setHandler(JsonJettyRPCFramework.newHandler[SomeApi](impl))
    server.start()

    val httpClient = new HttpClient()
    httpClient.start()

    val rpc = JsonJettyRPCFramework.newClient[SomeApi](httpClient, s"http://localhost:$port/")

    (for {
      _ <- rpc.topper.initialize
      keks <- rpc.keks
      top <- rpc.isTop(keks)
      topKeks <- rpc.topper.topKeks
      topIsTop <- rpc.isTop(topKeks)
      err <- rpc.erroneousKeks.failed
    } yield {
      println(
        s"""
           |rpc.keks = $keks
           |rpc.isTop(rpc.keks) = $top
           |rpc.topper.topKeks = $topKeks
           |rpc.isTop(rpc.topper.topKeks) = $topIsTop
           |rpc.erroneousTopper = $err
         """.stripMargin.trim)
    }) onComplete { _ =>
      server.stop()
      httpClient.stop()
    }
  }
}
