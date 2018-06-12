package com.avsystem.commons
package rpc.akka

import com.avsystem.commons.rpc.akka.AkkaRPCFramework._
import monix.reactive.Observable


/**
  * @author Wojciech Milewski
  */
trait TestRPC {
  def fireAndForget(): Unit
  def echoAsString(int: Int): Future[String]
  def stream: Observable[Int]
  def inner: InnerRPC
}
object TestRPC extends RPCCompanion[TestRPC]

trait InnerRPC {
  def innerFire(): Unit
}
object InnerRPC extends RPCCompanion[InnerRPC]
