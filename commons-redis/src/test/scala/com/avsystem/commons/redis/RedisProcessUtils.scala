package com.avsystem.commons
package redis

import java.io.ByteArrayInputStream

import com.avsystem.commons.redis.commands.NodeId
import org.scalatest.Suite

import scala.concurrent.duration._
import scala.sys.process._

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait RedisProcessUtils extends UsesActorSystem { this: Suite =>
  def redisHome = sys.env.getOpt("REDIS_HOME")
  def inRedisHome(cmd: String) = redisHome.fold(cmd)(_ + "/" + cmd)
  def password = Opt.empty[String]

  private val NodeLogRegex = ".*Node configuration loaded, I'm ([0-9a-f]+)$".r

  case class RedisProcess(process: Process, pid: Int, nodeId: Opt[NodeId]) {
    def stop() = s"kill -SIGSTOP $pid".!!
    def cont() = s"kill -SIGCONT $pid".!!
    def term() = s"kill -SIGTERM $pid".!!
    def kill() = s"kill -SIGKILL $pid".!!
  }

  def launchRedis(arguments: String*): Future[RedisProcess] = {
    val promise = Promise[Unit]()
    var pid = 0
    var nodeId: Opt[NodeId] = Opt.Empty
    val passArgs = password.fold(Seq.empty[String])(p => Seq("--requirepass", p))
    val process = (inRedisHome("redis-server") +: arguments ++: passArgs).run(ProcessLogger { line =>
      actorSystem.log.debug(line)
      line match {
        case NodeLogRegex(rawNodeId) =>
          nodeId = NodeId(rawNodeId).opt
        case _ =>
      }
      if (line.contains("* The server is now ready to accept connections on port")) {
        pid = line.split(":", 2).head.toInt
        promise.success(())
      }
    })
    promise.future.mapNow(_ => RedisProcess(process, pid, nodeId))
  }

  private def scheduleSpawn(delay: FiniteDuration)(fun: => Unit) =
    actorSystem.scheduler.scheduleOnce(delay)(fun)(SeparateThreadExecutionContext)

  def shutdownRedis(port: Int, process: RedisProcess): Future[Unit] =
    SeparateThreadExecutionContext.submit {
      val shutdownScript =
        """
          |CONFIG SET appendonly no
          |SHUTDOWN NOSAVE
        """.stripMargin

      val passOption = password.map(p => Seq("-a", p)).getOrElse(Seq.empty)
      val command = Seq(inRedisHome("redis-cli"), "-p", port.toString) ++ passOption
      def input = new ByteArrayInputStream(shutdownScript.getBytes)
      (command #< input).run(ProcessLogger(_ => (), err => Console.err.println(err)))
      val tc = scheduleSpawn(3.seconds)(process.term())
      val kc = scheduleSpawn(6.seconds)(process.kill())
      process.process.exitValue()
      tc.cancel()
      kc.cancel()
      ()
    }
}
