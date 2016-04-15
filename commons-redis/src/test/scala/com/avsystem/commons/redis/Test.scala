package com.avsystem.commons
package redis

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.avsystem.commons.redis.commands.RedisNodeCommands

import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Author: ghik
  * Created: 05/04/16.
  */
object Test {
  implicit val as = ActorSystem()
  implicit val ec: ExecutionContext = as.dispatcher
  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  implicit class FutureGet[A](private val fut: Future[A]) extends AnyVal {
    def get = Await.result(fut, timeout.duration)
  }

  implicit class StringOps(private val s: String) extends AnyVal {
    def bytes = ByteString(s)
  }

  val rc = new RedisNodeClient()
  val commands = RedisNodeCommands.operations.executedWith(rc)
}
