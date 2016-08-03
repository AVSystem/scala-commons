package com.avsystem.commons
package redis

import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import scala.sys.process._

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesRedisServer extends BeforeAndAfterAll with RedisProcessUtils {this: Suite =>
  def port = 7000
  def address = NodeAddress(port = port)

  var redisProcess: Process = _
  val initPromise = Promise[Unit]()

  override protected def beforeAll() = {
    super.beforeAll()
    redisProcess = Await.result(
      launchRedis(
        "--daemonize", "no",
        "--port", port.toString
      ),
      10.seconds
    )
  }

  override protected def afterAll() = {
    shutdownRedis(port, redisProcess)
    Thread.sleep(1000) // ensure that port is released
    super.afterAll()
  }
}
