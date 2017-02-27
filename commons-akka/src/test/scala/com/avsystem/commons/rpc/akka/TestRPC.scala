package com.avsystem.commons
package rpc.akka

import com.avsystem.commons.rpc.RPC
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

@RPC
trait InnerRPC {
  def innerFire(): Unit
}