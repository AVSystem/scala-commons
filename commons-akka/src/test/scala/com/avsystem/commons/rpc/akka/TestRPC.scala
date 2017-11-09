package com.avsystem.commons
package rpc.akka

import com.avsystem.commons.rpc.RPC
import com.avsystem.commons.rpc.akka.AkkaRPCFramework._
import monix.reactive.Observable


/**
  * @author Wojciech Milewski
  */
@RPC
trait TestRPC {
  def fireAndForget(): Unit
  def echoAsString(int: Int): Future[String]
  def stream: Observable[Int]
  def inner: InnerRPC
}
object TestRPC {
  implicit val fullRPCInfo: BaseFullRPCInfo[TestRPC] = materializeFullInfo
}

@RPC
trait InnerRPC {
  def innerFire(): Unit
}
object InnerRPC {
  implicit val fullRPCInfo: BaseFullRPCInfo[InnerRPC] = materializeFullInfo
}
