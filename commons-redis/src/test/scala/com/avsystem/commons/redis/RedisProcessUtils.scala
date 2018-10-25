package com.avsystem.commons
package redis

import com.avsystem.commons.redis.commands.NodeId
import org.scalatest.Suite

import scala.concurrent.duration._
import scala.sys.process._

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait RedisProcessUtils extends UsesActorSystem { this: Suite =>
  def redisHome: Opt[String] = sys.env.getOpt("REDIS_HOME")
  def inRedisHome(cmd: String): String = redisHome.fold(cmd)(_ + "/" + cmd)
  def password: Opt[String] = Opt.empty[String]
  def runCommand: List[String] =
    if (System.getProperty("os.name") == "Windows 10") List("ubuntu", "run") else Nil

  private val NodeLogRegex = ".*Node configuration loaded, I'm ([0-9a-f]+)$".r

  case class RedisProcess(process: Process, pid: Int, nodeId: Opt[NodeId])

  def launchRedis(arguments: String*): Future[RedisProcess] = {
    val promise = Promise[Unit]()
    var pid = 0
    var nodeId: Opt[NodeId] = Opt.Empty
    val passArgs = password.fold(Seq.empty[String])(p => Seq("--requirepass", p))
    val process = (runCommand ++: inRedisHome("redis-server") +: arguments ++: passArgs).run(
      ProcessLogger { line =>
        actorSystem.log.debug(line)
        line match {
          case NodeLogRegex(rawNodeId) =>
            nodeId = NodeId(rawNodeId).opt
          case _ =>
        }
        if (line.contains("* Ready to accept connections")) {
          pid = line.split(":", 2).head.toInt
          promise.success(())
        }
      })
    promise.future.mapNow(_ => RedisProcess(process, pid, nodeId))
  }

  def shutdownRedis(port: Int, process: RedisProcess): Future[Unit] =
    SeparateThreadExecutionContext.submit {
      actorSystem.log.info(s"Killing Redis process ${process.pid}")
      (runCommand ++ List("kill", "-SIGKILL", process.pid.toString))
        .run(ProcessLogger(actorSystem.log.debug(_), actorSystem.log.error(_)))
      process.process.exitValue()
      ()
    }
}
