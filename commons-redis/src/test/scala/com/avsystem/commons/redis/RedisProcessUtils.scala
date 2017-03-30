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
  def redisHome = sys.env("REDIS_HOME")
  def password = Opt.empty[String]

  private val NodeLogRegex = ".*Node configuration loaded, I'm ([0-9a-f]+)$".r

  case class RedisProcess(process: Process, pid: Int, nodeId: Opt[NodeId]) {
    def stop() = Seq(s"kill -SIGSTOP $pid").!!
    def cont() = Seq(s"kill -SIGCONT $pid").!!
    def term() = Seq(s"kill -SIGTERM $pid").!!
    def kill() = Seq(s"kill -SIGKILL $pid").!!
  }

  def launchRedis(arguments: String*): Future[RedisProcess] = {
    val promise = Promise[Unit]()
    var pid = 0
    var nodeId: Opt[NodeId] = Opt.Empty
    val passArgs = password.fold(Seq.empty[String])(p => Seq("--requirepass", p))
    val process = (s"$redisHome/redis-server" +: arguments ++: passArgs).run(ProcessLogger { line =>
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

  def shutdownRedis(port: Int, process: RedisProcess): Future[Unit] = Future({
    val shutdownScript =
      """
        |CONFIG SET appendonly no
        |SHUTDOWN NOSAVE
      """.stripMargin

    val passOption = password.map(p => Seq("-a", p)).getOrElse(Seq.empty)
    val command = Seq(s"$redisHome/redis-cli", "-p", port.toString) ++ passOption
    def input = new ByteArrayInputStream(shutdownScript.getBytes)
    (command #< input).!! // please shut down
    val tc = scheduleSpawn(3.seconds)(process.term()) // dude, stop
    val kc = scheduleSpawn(6.seconds)(process.kill()) // just die already
    process.process.exitValue()
    tc.cancel()
    kc.cancel()
    ()
  })(SeparateThreadExecutionContext)
}
