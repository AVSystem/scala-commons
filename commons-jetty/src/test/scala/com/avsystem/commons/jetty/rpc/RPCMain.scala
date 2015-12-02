package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rpc.{AsRawRPC, AsRealRPC, RPC}
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.server.Server

import scala.concurrent.Future

/**
  * @author MKej
  */
object RPCMain {
  trait SomeApi extends RPC {
    def keks: Future[Int]
    def isTop(keks: Int): Future[Boolean]
    def topper: Topper
  }

  trait Topper extends RPC {
    def initialize: Future[Unit]
    def topKeks: Future[Int]
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
    }

    val port = 1337

    val server = new Server(port)
    server.setHandler(new RPCHandler(AsRawRPC[SomeApi].asRaw(impl)))
    server.start()

    val httpClient = new HttpClient()
    httpClient.start()

    val rpc = AsRealRPC[SomeApi].asReal(new RPCClient(httpClient, s"http://localhost:$port/", ""))

    (for {
      _ <- rpc.topper.initialize
      keks <- rpc.keks
      top <- rpc.isTop(keks)
      topKeks <- rpc.topper.topKeks
      topIsTop <- rpc.isTop(topKeks)
    } yield {
      println(
        s"""
           |rpc.keks = $keks
           |rpc.isTop(rpc.keks) = $top
           |rpc.topper.topKeks = $topKeks
           |rpc.isTop(rpc.topper.topKeks) = $topIsTop
         """.stripMargin.trim)
    }) onComplete { _ =>
      server.stop()
      httpClient.stop()
    }
  }
}
