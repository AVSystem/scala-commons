package com.avsystem.test.client

import akka.actor.{ActorPath, ActorSystem}
import akka.stream.ActorMaterializer
import com.avsystem.commons.rpc.akka.{AkkaRPCClientConfig, AkkaRPCFramework}
import com.avsystem.test.api.ServerRPC
import monifu.concurrent.Implicits.globalScheduler

import scala.util.{Failure, Success}

/**
  * @author Wojciech Milewski
  */
object Main {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    val serverRPC = AkkaRPCFramework.client[ServerRPC](AkkaRPCClientConfig(serverPath = ActorPath.fromString("akka.tcp://serverSystem@127.0.0.1:2552/user/rpcServerActor")))
    serverRPC.unitMethod(Some("ok"))
    serverRPC.unitMethod(None)
    serverRPC.futureMethod("string")(5).onComplete {
      case Success(value) => println(value)
      case Failure(e) => e.printStackTrace()
    }
    serverRPC.publisherMethod(10).foreach(println)
  }

}
