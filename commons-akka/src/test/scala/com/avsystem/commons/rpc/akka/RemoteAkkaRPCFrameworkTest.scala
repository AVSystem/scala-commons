package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorPath, ActorSystem}
import com.typesafe.config.{Config, ConfigFactory}

/**
  * @author Wojciech Milewski
  */
class RemoteAkkaRPCFrameworkTest extends AkkaRPCFrameworkTest(
  serverSystem = ActorSystem("ServerRPC", RemoteAkkaRPCFrameworkTest.config(2552)),
  clientSystem = ActorSystem("ClientRPC", RemoteAkkaRPCFrameworkTest.config(2553)),
  existingPath = Some(ActorPath.fromString("akka.tcp://ServerRPC@127.0.0.1:2552/user/rpcServerActor")),
  nonExistingPath = Some(ActorPath.fromString("akka.tcp://user@127.0.0.1:2553/thisactorshouldnotexists"))
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
