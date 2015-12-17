package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rpc.RPC
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.server.Server
import upickle.Js

import scala.concurrent.Future

/**
  * @author MKej
  */
object RPCMain {
  @RPC trait SomeApi {
    def keks: Future[Int]
    def isTop(keks: Int): Future[Boolean]
    def topper: Topper
  }

  @RPC trait Topper {
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

    object jettyFramework extends JettyRPCFramework {
      type RawValue = Js.Value
      type Reader[T] = upickle.default.Reader[T]
      type Writer[T] = upickle.default.Writer[T]

      def valueToJson(value: RawValue) = upickle.json.write(value)
      def jsonToValue(json: String) = upickle.json.read(json)
      def argsToJson(args: List[List[RawValue]]) = upickle.json.write(argsToJsArr(args))
      def jsonToArgs(json: String) = jsArrToArgs(upickle.json.read(json))

      def read[T: Reader](raw: RawValue): T = upickle.default.readJs[T](raw)
      def write[T: Writer](value: T): RawValue = upickle.default.writeJs[T](value)

      def argsToJsArr(argLists: List[List[Js.Value]]): Js.Value = {
        Js.Arr(argLists map { args => Js.Arr(args: _*) }: _*)
      }

      def jsArrToArgs(value: Js.Value): List[List[Js.Value]] = {
        value match {
          case array: Js.Arr =>
            (array.value map {
              case nestedArray: Js.Arr => nestedArray.value.toList
              case _ => List()
            }).toList
          case _ => List()
        }
      }
    }

    val port = 1337

    val server = new Server(port)
    server.setHandler(jettyFramework.newHandler[SomeApi](impl))
    server.start()

    val httpClient = new HttpClient()
    httpClient.start()

    val rpc = jettyFramework.newClient[SomeApi](httpClient, s"http://localhost:$port/")

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
