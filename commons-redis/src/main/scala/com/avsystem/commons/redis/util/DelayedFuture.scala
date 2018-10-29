package com.avsystem.commons
package redis.util

import akka.actor.ActorSystem
import com.avsystem.commons.concurrent.RunNowEC

import scala.concurrent.duration.{Duration, FiniteDuration}

object DelayedFuture {
  def apply(delay: FiniteDuration)(implicit system: ActorSystem): Future[Unit] =
    if (delay <= Duration.Zero) Future.unit
    else {
      val promise = Promise[Unit]()
      system.scheduler.scheduleOnce(delay)(promise.success(()))(RunNowEC)
      promise.future
    }
}
