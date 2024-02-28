package com.avsystem.commons
package redis

import org.apache.pekko.actor.{ActorSystem, Props}
import org.apache.pekko.util.Timeout
import com.avsystem.commons.concurrent.RetryStrategy
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.actor.SentinelsMonitoringActor
import com.avsystem.commons.redis.config.{ExecutionConfig, MasterSlaveConfig}
import com.avsystem.commons.redis.exception.{ClientStoppedException, NoMasterException, NodeRemovedException}
import com.avsystem.commons.redis.monitoring.SentinelStateObserver
import com.avsystem.commons.redis.protocol.{ErrorMsg, RedisReply, TransactionReply}
import com.avsystem.commons.redis.util.DelayedFuture

/**
  * Redis client implementation for master-slave installations with Redis Sentinels.
  * [[RedisMasterSlaveClient]] is able to execute the same set of commands as [[RedisNodeClient]].
  *
  * @param masterName            name of the master, as configured in the sentinels
  * @param seedSentinels         sentinel seed addresses - must point to at least one reachable sentinel
  * @param config                client configuration - [[MasterSlaveConfig]]
  * @param sentinelStateObserver optional observer of client's state and connections - [[SentinelStateObserver]]
  */
final class RedisMasterSlaveClient(
  val masterName: String,
  val seedSentinels: Seq[NodeAddress] = Seq(NodeAddress.DefaultSentinel),
  val config: MasterSlaveConfig = MasterSlaveConfig(),
  val sentinelStateObserver: OptArg[SentinelStateObserver] = OptArg.Empty,
)(implicit system: ActorSystem) extends RedisClient with RedisNodeExecutor {

  require(seedSentinels.nonEmpty, "No seed sentinel nodes provided")

  // (optimization) allows us to avoid going through `initPromise` after the client has been successfully initialized
  @volatile private[this] var initSuccess = false
  @volatile private[this] var master: RedisNodeClient = _
  @volatile private[this] var masterListener = (_: RedisNodeClient) => ()
  // ensures that all operations fail fast after client is closed instead of being sent further
  @volatile private[this] var failure = Opt.empty[Throwable]

  private val initPromise = Promise[Unit]()
  initPromise.future.foreachNow(_ => initSuccess = true)

  private def ifReady[T](code: => Future[T]): Future[T] = failure match {
    case Opt.Empty if initSuccess => code
    case Opt.Empty => initPromise.future.flatMapNow(_ => code)
    case Opt(t) => Future.failed(t)
  }

  private def onNewMaster(newMaster: RedisNodeClient): Unit = {
    master = newMaster
    masterListener(master)
    sentinelStateObserver.foreach(_.onMasterChange(newMaster.address))
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
    system.actorOf(Props(new SentinelsMonitoringActor(masterName, seedSentinels, config, initPromise.failure, onNewMaster, sentinelStateObserver)))

  def setMasterListener(listener: RedisNodeClient => Unit)(implicit executor: ExecutionContext): Unit =
    masterListener = newMaster => executor.execute(jRunnable(listener(newMaster)))

  def currentMaster: Opt[RedisNodeClient] = master.opt

  def initializedCurrentMaster: Future[RedisNodeClient] =
    initPromise.future.mapNow(_ => master)

  def initialized: Future[this.type] =
    initPromise.future.mapNow(_ => this)

  def executionContext: ExecutionContext = system.dispatcher

  def executeBatch[A](batch: RedisBatch[A], execConfig: ExecutionConfig): Future[A] = ifReady {
    implicit val timeout: Timeout = execConfig.responseTimeout
    val packs = batch.rawCommandPacks
    val replyFut = handleFailover(master.executeRaw(packs), packs, config.failoverBackoutStrategy)
    replyFut.map(batch.decodeReplies(_))(execConfig.decodeOn)
  }

  private def handleFailover(
    resultFut: Future[PacksResult], packs: RawCommandPacks, retryStrategy: RetryStrategy
  )(implicit timeout: Timeout): Future[PacksResult] =
    resultFut.flatMapNow { pr =>
      pr.collectFirstOpt { case ReadonlyReply(err) => err } match {
        case Opt.Empty => resultFut
        case Opt(err) =>
          // Got READONLY error for at least one command, this means failover is in progress and we should soon obtain
          // information about new master. Until that happens, back out and retry.
          // NOTE: it is safe (and better) to retry _all_ commands in the batch - the ones that didn't fail are read-only.
          retryStrategy.nextRetry match {
            case Opt.Empty =>
              Future.successful(PacksResult.Failure(new NoMasterException(err)))
            case Opt((delay, nextStrategy)) =>
              DelayedFuture(delay).flatMapNow(_ => handleFailover(master.executeRaw(packs), packs, nextStrategy))
          }
      }
    } recoverWithNow {
      case _: NodeRemovedException =>
        // this means the master changed before the batch could be sent, retry immediately
        handleFailover(master.executeRaw(packs), packs, config.failoverBackoutStrategy)
    }

  def executeOp[A](op: RedisOp[A], executionConfig: ExecutionConfig): Future[A] =
    ifReady(master.executeOp(op, executionConfig))

  def close(): Unit = {
    failure = new ClientStoppedException(Opt.Empty).opt
    system.stop(monitoringActor)
  }
}

object ReadonlyReply {
  def unapply(reply: RedisReply): Opt[ErrorMsg] = reply match {
    case err: ErrorMsg if err.errorCode == "READONLY" => Opt(err)
    case TransactionReply(elements) => elements.headOpt.flatMap(unapply)
    case _ => Opt.Empty
  }
}
