package com.avsystem.commons
package redis

import akka.actor.ActorSystem
import akka.util.Timeout
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Milliseconds, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesActorSystem extends BeforeAndAfterAll with PatienceConfiguration {this: Suite =>
  implicit lazy val actorSystem: ActorSystem = ActorSystem()
  implicit def executionContext: ExecutionContext = actorSystem.dispatcher
  implicit val timeout = Timeout(60.seconds)

  override implicit def patienceConfig =
    PatienceConfig(Span(60, Seconds), Span(100, Milliseconds))

  override protected def afterAll() = {
    Await.ready(actorSystem.terminate(), Duration.Inf)
    super.afterAll()
  }
}
