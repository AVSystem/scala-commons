package com.avsystem.test.server.api

import com.avsystem.test.api.{EchoRPC, ServerRPC}
import monifu.reactive.Observable

import scala.concurrent.Future

/**
  * @author Wojciech Milewski
  */
class EchoRPCImpl extends EchoRPC {
  override def stringEcho(item: String): Future[String] = Future.successful(item)
  override def intEcho(item: Int): Future[Int] = Future.successful(item)
}

class ServerRPCImpl extends ServerRPC {
  override def unitMethod(option: Option[String]): Unit = {
    println(s"called unit method: ${option.getOrElse("_")}")
  }
  override def futureMethod(string: String)(int: Int): Future[String] = Future.successful(s"called future method; $string; $int")
  override def publisherMethod(limit: Int): Observable[String] = {
//    Observable.intervalAtFixedRate(1.second).take(limit).map(_.toString)
    Observable.from(1, 2, 3, 4, 5).map(_.toString)
  }
  def echoRPC: EchoRPC = new EchoRPCImpl
}
