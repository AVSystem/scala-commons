package com.avsystem.commons
package redis.examples

import akka.actor.ActorSystem
import akka.util.Timeout
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Basic example showing how to execute simple command on [[RedisConnectionClient]].
  */
object ConnectionClientExample extends App {
  implicit val actorSystem = ActorSystem()
  implicit val timeout: Timeout = 10.seconds

  // Connection client only uses a single, non-reconnectable connection
  val client = new RedisConnectionClient
  // but exposes API to manipulate that connection
  val api = RedisApi.Connection.Async.StringTyped(client)

  // for example, we can execute CLIENT GETNAME, which we cannot execute using RedisNodeClient
  api.clientGetname.onComplete {
    case Success(Opt(name)) => println(s"Connection name is $name")
    case Success(Opt.Empty) => println("No connection name")
    case Failure(t) => t.printStackTrace()
  }
}
