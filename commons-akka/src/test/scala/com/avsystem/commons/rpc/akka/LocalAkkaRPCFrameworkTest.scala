package com.avsystem.commons
package rpc.akka
import akka.actor.ActorSystem
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike}

/**
  * @author Wojciech Milewski
  */
class LocalAkkaRPCFrameworkTest(system: ActorSystem) extends AkkaRPCFrameworkTest(system, system) {
  def this() = this(ActorSystem())
}
