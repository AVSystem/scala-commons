package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorPath, ActorSystem}
import com.typesafe.config.ConfigFactory

/**
  * @author Wojciech Milewski
  */
class RemoteAkkaRPCFrameworkTest extends AkkaRPCFrameworkTest(
  serverSystem = ActorSystem("ServerBenchmark", ConfigFactory.parseString(
    s"""
       |akka {
       |  actor {
       |    provider = "akka.remote.RemoteActorRefProvider"
       |  }
       |  remote {
       |    enabled-transports = ["akka.remote.netty.tcp"]
       |    netty.tcp {
       |      hostname = "127.0.0.1"
       |      port = 2552
       |    }
       |  }
       |}
       """.stripMargin)),
  clientSystem = ActorSystem("ClientBenchmark", ConfigFactory.parseString(
    s"""
       |akka {
       |  actor {
       |    provider = "akka.remote.RemoteActorRefProvider"
       |  }
       |  remote {
       |    enabled-transports = ["akka.remote.netty.tcp"]
       |    netty.tcp {
       |      hostname = "127.0.0.1"
       |      port = 2553
       |    }
       |  }
       |}
       """.stripMargin)),
  existingPath = Some(ActorPath.fromString("akka.tcp://ServerBenchmark@127.0.0.1:2552/user/rpcServerActor")),
  nonExistingPath = Some(ActorPath.fromString("akka.tcp://user/thisactorshouldnotexists"))
)
