package com.avsystem.commons
package redis

import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.sys.process._

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesRedisServer extends BeforeAndAfterAll {this: Suite =>
  def port = 7000
  def address = NodeAddress(port = port)

  var redisProcess: Process = _

  override protected def beforeAll() = {
    super.beforeAll()
    redisProcess = Seq("redis-server",
      "--daemonize", "no",
      "--port", port.toString
    ).run()
    Thread.sleep(3000)
  }

  override protected def afterAll() = {
    redisProcess.destroy()
    Thread.sleep(500)
    super.afterAll()
  }
}
