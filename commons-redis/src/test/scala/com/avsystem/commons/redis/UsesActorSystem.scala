package com.avsystem.commons
package redis

import akka.actor.ActorSystem
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesActorSystem extends BeforeAndAfterAll {this: Suite =>
  implicit lazy val actorSystem: ActorSystem = ActorSystem()
  implicit def executionContext: ExecutionContext = actorSystem.dispatcher
  implicit val timeout = Timeout(10.seconds)

  override protected def afterAll() = {
    Await.ready(actorSystem.terminate(), Duration.Inf)
    super.afterAll()
  }
}
