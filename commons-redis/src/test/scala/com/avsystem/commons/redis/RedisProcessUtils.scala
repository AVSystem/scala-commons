package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.sys.process._

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait RedisProcessUtils {
  implicit def executionContext: ExecutionContext

  def redisHome = sys.env("REDIS_HOME")
  def password = Opt.empty[String]

  def launchRedis(arguments: String*): Future[Process] = {
    val promise = Promise[Unit]()
    val passArgs = password.map(p => Seq("--requirepass", p)).getOrElse(Nil)
    val process = (s"$redisHome/redis-server" +: arguments ++: passArgs).run(ProcessLogger { line =>
      println(line)
      if (line.contains("* The server is now ready to accept connections on port")) {
        promise.success(())
      }
    })
    promise.future.mapNow(_ => process)
  }

  def shutdownRedis(port: Int, process: Process): Future[Unit] = Future {
    val passOption = password.map(p => Seq("-a", p)).getOrElse(Seq.empty)
    (Seq(s"$redisHome/redis-cli", "-p", port.toString) ++ passOption :+ "SHUTDOWN").!!
    process.exitValue()
  }
}
