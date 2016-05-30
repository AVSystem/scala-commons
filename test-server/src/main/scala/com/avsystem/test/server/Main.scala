package com.avsystem.test.server

import akka.actor.ActorSystem
import com.avsystem.rpc.akka.AkkaRPCFramework
import com.avsystem.test.api.ServerRPC
import com.avsystem.test.server.api.ServerRPCImpl

/**
  * @author Wojciech Milewski
  */
object Main {

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("serverSystem")
    val actor = AkkaRPCFramework.serverActor[ServerRPC](new ServerRPCImpl)
    println(actor.path)
  }

}
