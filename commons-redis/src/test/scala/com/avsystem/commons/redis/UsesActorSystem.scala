package com.avsystem.commons
package redis

import akka.actor.ActorSystem
import akka.util.Timeout
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Milliseconds, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesActorSystem extends BeforeAndAfterAll with PatienceConfiguration { this: Suite =>
  implicit lazy val actorSystem: ActorSystem = ActorSystem()
  implicit def executionContext: ExecutionContext = actorSystem.dispatcher
  implicit val timeout = Timeout(60.seconds)

  override implicit def patienceConfig =
    PatienceConfig(scaled(Span(60, Seconds)), scaled(Span(10, Milliseconds)))

  override protected def afterAll() = {
    Await.ready(actorSystem.terminate(), Duration.Inf)
    super.afterAll()
  }

  def wait(duration: FiniteDuration): Future[Unit] =
    if (duration == Duration.Zero) Future.successful(()) else {
      val promise = Promise[Unit]()
      actorSystem.scheduler.scheduleOnce(duration)(promise.success(()))
      promise.future
    }

  def waitUntil(predicate: => Future[Boolean], retryInterval: FiniteDuration): Future[Unit] =
    predicate.flatMap { r =>
      if (r) Future.successful(())
      else wait(retryInterval).flatMap(_ => waitUntil(predicate, retryInterval))
    }

  def waitFor[T](future: => Future[T])(condition: T => Boolean, retryInterval: FiniteDuration): Future[T] =
    future.flatMap { value =>
      if (condition(value)) Future.successful(value)
      else wait(retryInterval).flatMap(_ => waitFor(future)(condition, retryInterval))
    }
}
