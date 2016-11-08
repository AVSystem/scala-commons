package com.avsystem.commons
package rpc.akka

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.Ignore

/**
  * @author Wojciech Milewski
  */
// Ignored because `monix.execution.Ack.Continue.transformWith` blows stack for Scala 2.12
// This is already fixed in Monix but not yet released (as of Monix 2.0.6)
@Ignore
class LocalAkkaRPCFrameworkTest private(system: ActorSystem) extends AkkaRPCFrameworkTest(system, system) {
  def this() = this(ActorSystem("LocalRPC", ConfigFactory.parseString(
    s"""
       |akka {
       |  loglevel = "DEBUG"
       |}
     """.stripMargin)))


}
