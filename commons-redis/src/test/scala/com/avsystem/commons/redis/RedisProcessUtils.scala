package com.avsystem.commons
package redis

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.sys.process._

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait RedisProcessUtils {
  implicit def executionContext: ExecutionContext

  def redisHome = sys.env("REDIS_HOME")

  def launchRedis(arguments: String*): Future[Process] = {
    val promise = Promise[Unit]()
    val process = (s"$redisHome/redis-server" +: arguments).run(ProcessLogger { line =>
      println(line)
      if (line.contains("* The server is now ready to accept connections on port")) {
        promise.success(())
      }
    })
    promise.future.mapNow(_ => process)
  }

  def shutdownRedis(port: Int, process: Process): Future[Unit] = Future {
    Seq(s"$redisHome/redis-cli", "-p", port.toString, "SHUTDOWN").!!
    process.exitValue()
  }
}
