package com.avsystem.commons
package rpc.akka

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

/**
  * @author Wojciech Milewski
  */
class LocalAkkaRPCFrameworkTest private(system: ActorSystem) extends AkkaRPCFrameworkTest(system, system) {
  def this() = this(ActorSystem("LocalRPC", ConfigFactory.parseString(
    s"""
       |akka {
       |  loglevel = "DEBUG"
       |}
     """.stripMargin)))


}
