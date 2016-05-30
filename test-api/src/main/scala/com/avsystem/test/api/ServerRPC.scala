package com.avsystem.test.api

import com.avsystem.commons.rpc.RPC
import monifu.reactive.Observable

import scala.concurrent.Future

/**
  * @author Wojciech Milewski
  */
@RPC
trait EchoRPC {
  def stringEcho(item: String): Future[String]
  def intEcho(item: Int): Future[Int]
}

@RPC
trait ServerRPC {
  def unitMethod(option: Option[String]): Unit
  def futureMethod(string: String)(int: Int): Future[String]
  def publisherMethod(limit: Int): Observable[String]
  //todo implement EchoRPC - problem with macro that returns GenericRPCFramework.this.RawValue instead of AkkaRPCFramework.RawValue
}