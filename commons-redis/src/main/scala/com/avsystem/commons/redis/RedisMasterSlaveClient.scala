package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.actor.SentinelsMonitoringActor
import com.avsystem.commons.redis.config.{ExecutionConfig, MasterSlaveConfig}
import com.avsystem.commons.redis.exception.{ClientStoppedException, NodeRemovedException}

final class RedisMasterSlaveClient(
  val masterName: String,
  val seedSentinels: Seq[NodeAddress] = Seq(NodeAddress.DefaultSentinel),
  val config: MasterSlaveConfig = MasterSlaveConfig()
)(implicit system: ActorSystem) extends RedisKeyedExecutor with Closeable {

  require(seedSentinels.nonEmpty, "No seed sentinel nodes provided")

  // (optimization) allows us to avoid going through `initPromise` after the client has been successfully initialized
  @volatile private[this] var initSuccess = false
  @volatile private[this] var master: RedisNodeClient = _
  @volatile private[this] var masterListener = (_: RedisNodeClient) => ()
  // ensures that all operations fail fast after client is closed instead of being sent further
  @volatile private[this] var failure = Opt.empty[Throwable]

  private val initPromise = Promise[Unit]
  initPromise.future.foreachNow(_ => initSuccess = true)

  private def ifReady[T](code: => Future[T]): Future[T] = failure match {
    case Opt.Empty if initSuccess => code
    case Opt.Empty => initPromise.future.flatMapNow(_ => code)
    case Opt(t) => Future.failed(t)
  }

  private def onNewMaster(newMaster: RedisNodeClient): Unit = {
    master = newMaster
    masterListener(master)
    if (!initSuccess) {
      import system.dispatcher
      newMaster.initialized.onComplete { result =>
        // this check handles situation when master changed _exactly_ during initialization, see comment for
        // analogous situation in RedisClusterClient
        if (master eq newMaster) {
          initPromise.tryComplete(result.map(_ => ()))
        }
      }
    }
  }

  private val monitoringActor =
    system.actorOf(Props(new SentinelsMonitoringActor(masterName, seedSentinels, config, initPromise.failure, onNewMaster)))

  def setMasterListener(listener: RedisNodeClient => Unit)(implicit executor: ExecutionContext): Unit =
    masterListener = newMaster => executor.execute(jRunnable(listener(newMaster)))

  def currentMaster: Opt[RedisNodeClient] = master.opt

  def initializedCurrentMaster: Future[RedisNodeClient] =
    initPromise.future.mapNow(_ => master)

  def initialized: Future[this.type] =
    initPromise.future.mapNow(_ => this)

  def executionContext: ExecutionContext = system.dispatcher

  def executeBatch[A](batch: RedisBatch[A], config: ExecutionConfig): Future[A] = {
    implicit val timeout: Timeout = config.responseTimeout
    batch.rawCommandPacks.requireLevel(Level.Node, "ClusterClient")
    ifReady {
      master.executeBatch(batch, config).recoverWithNow {
        case _: NodeRemovedException => executeBatch(batch, config)
      }
    }
  }

  def close(): Unit = {
    failure = new ClientStoppedException(Opt.Empty).opt
    system.stop(monitoringActor)
  }
}
