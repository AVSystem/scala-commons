package com.avsystem.commons
package redis

import com.avsystem.commons.redis.commands.NodeId
import org.scalatest.Suite

import scala.sys.process._

/** Author: ghik Created: 27/06/16.
  */
trait RedisProcessUtils extends UsesActorSystem { this: Suite =>
  def redisHome: Opt[String] = sys.env.getOpt("REDIS_HOME")
  def inRedisHome(cmd: String): String = redisHome.fold(cmd)(_ + "/" + cmd)
  def password: Opt[String] = Opt.empty[String]
  def runCommand: List[String] =
    if (System.getProperty("os.name") == "Windows 10") List("ubuntu", "run") else Nil

  private val ReadyRegex = ".*Ready to accept connections.*".r
  private val NodeLogRegex = ".*Node configuration loaded, I'm ([0-9a-f]+)$".r
  private val SentinelIdRegex = ".*Sentinel ID is ([0-9a-f]+)$".r
  private val PidRegex = ".*pid=([0-9]+).*".r

  case class RedisProcess(process: Process, pid: Int, nodeId: Opt[NodeId])

  def launchRedis(arguments: String*): Future[RedisProcess] =
    launchRedis("redis-server", arguments)

  def launchSentinel(arguments: String*): Future[RedisProcess] =
    launchRedis("redis-sentinel", arguments)

  def launchRedis(executable: String, arguments: Seq[String]): Future[RedisProcess] = {
    val promise = Promise[Unit]()
    var pid = -1
    var nodeId: Opt[NodeId] = Opt.Empty
    val passArgs = password.fold(Seq.empty[String])(p => Seq("--requirepass", p))
    val process = (runCommand ++: inRedisHome(executable) +: arguments ++: passArgs).run(ProcessLogger { line =>
      actorSystem.log.debug(line)
      line match {
        case PidRegex(pidStr) =>
          pid = pidStr.toInt
        case NodeLogRegex(rawNodeId) =>
          nodeId = NodeId(rawNodeId).opt
        case SentinelIdRegex(rawSentinelId) =>
          nodeId = NodeId(rawSentinelId).opt
          promise.success(())
        case ReadyRegex() =>
          promise.success(())
        case _ =>
      }
    })
    promise.future.mapNow { _ =>
      if (pid < 0) {
        throw new IllegalStateException("Could not determine Redis process PID")
      }
      RedisProcess(process, pid, nodeId)
    }
  }

  def shutdownRedis(process: RedisProcess): Future[Unit] =
    SeparateThreadExecutionContext.submit {
      actorSystem.log.info(s"Killing Redis process ${process.pid}")
      (runCommand ++ List("kill", "-SIGKILL", process.pid.toString))
        .run(ProcessLogger(actorSystem.log.debug(_), actorSystem.log.error(_)))
      process.process.exitValue()
      ()
    }
}
