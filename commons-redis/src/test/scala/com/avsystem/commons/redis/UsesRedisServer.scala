package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import scala.sys.process._

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesRedisServer extends BeforeAndAfterAll with RedisProcessUtils { this: Suite =>
  def port = 7000
  def address = NodeAddress(port = port)
  def password = Opt.empty[String]

  var redisProcess: Process = _
  val initPromise = Promise[Unit]()

  override protected def beforeAll() = {
    super.beforeAll()

    val passArgs = password.map(p => Seq("--requirepass", p)).getOrElse(Nil)
    val args = Seq(
      "--daemonize", "no",
      "--port", port.toString
    ) ++ passArgs

    redisProcess = Await.result(
      launchRedis(args: _*),
      10.seconds
    )
  }

  override protected def afterAll() = {
    shutdownRedis(port, redisProcess)
    Thread.sleep(1000) // ensure that port is released
    super.afterAll()
  }
}
