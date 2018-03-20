package com.avsystem.commons
package rpc.akka

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}

/**
  * @author Wojciech Milewski
  */
class RemoteAkkaRPCFrameworkTest extends AkkaRPCFrameworkTest(
  serverSystem = ActorSystem("ServerRPC", RemoteAkkaRPCFrameworkTest.config(port = 2552)),
  clientSystem = ActorSystem("ClientRPC", RemoteAkkaRPCFrameworkTest.config(port = 2553)),
  serverSystemPath = Some("akka.tcp://ServerRPC@127.0.0.1:2552")
)

object RemoteAkkaRPCFrameworkTest {
  def config(port: Int): Config = {
    ConfigFactory.parseString(
      s"""
         |akka {
         |  actor {
         |    provider = "akka.remote.RemoteActorRefProvider"
         |    serializers {
         |      remoteMessage = com.avsystem.commons.rpc.akka.serialization.RemoteMessageSerializer
         |    }
         |    serialization-bindings {
         |      "com.avsystem.commons.rpc.akka.RemoteMessage" = remoteMessage
         |    }
         |  }
         |  remote {
         |    enabled-transports = ["akka.remote.netty.tcp"]
         |    netty.tcp {
         |      hostname = "127.0.0.1"
         |      port = $port
         |    }
         |  }
         |}
       """.stripMargin)
  }
}
