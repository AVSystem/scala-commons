package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.concurrent.RunInQueueEC
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.RedisClusterClient.{AskingPack, CollectionPacks}
import com.avsystem.commons.redis.actor.ClusterMonitoringActor
import com.avsystem.commons.redis.actor.ClusterMonitoringActor.{GetClient, GetClientResponse, Refresh}
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.commands.{Asking, SlotRange}
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.exception._
import com.avsystem.commons.redis.protocol.{ErrorMsg, FailureReply, RedisMsg, RedisReply, TransactionReply}
import com.avsystem.commons.redis.util.SingletonSeq

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

/**
  * Redis client implementation for Redis Cluster deployments. Internally, it uses single [[RedisNodeClient]] instance
  * for every known master in the cluster plus a separate connection for monitoring of every known master.
  * [[RedisNodeClient]] instances are dynamically created and destroyed as the cluster changes its state.
  *
  * The cluster client is only able to execute commands or transactions containing keys which determine which master
  * should be contacted. However, it's possible to gain access to [[RedisNodeClient]] instances that [[RedisClusterClient]]
  * internally uses to connect to every master (see [[ClusterState]] and [[masterClient]]).
  *
  * [[RedisClusterClient]] can directly execute only [[RedisBatch]]es. It automatically distributes every batch
  * over multiple cluster masters and handles cluster redirections if necessary. See [[executeBatch]] for more
  * details.
  *
  * [[RedisClusterClient]] cannot directly execute [[RedisOp]]s (e.g. `WATCH`-`MULTI`-`EXEC` transactions).
  * You must manually access node client for appropriate master through [[currentState]] and execute the operation using it.
  * However, be aware that you must manually handle cluster redirections and cluster state changes while doing so.
  *
  * @param seedNodes nodes used to fetch initial cluster state from. You don't need to list all cluster nodes, it is
  *                  only required that at least one of the seed nodes is available during startup.
  */
final class RedisClusterClient(
  val seedNodes: Seq[NodeAddress] = List(NodeAddress.Default),
  val config: ClusterConfig = ClusterConfig())
  (implicit system: ActorSystem) extends RedisKeyedExecutor with Closeable {

  require(seedNodes.nonEmpty, "No seed nodes provided")

  // (optimization) allows us to avoid going through `initPromise` after the client has been successfully initialized
  @volatile private[this] var initSuccess = false
  @volatile private[this] var state = ClusterState(IndexedSeq.empty, Map.empty)
  @volatile private[this] var stateListener = (_: ClusterState) => ()
  // clients for nodes mentioned in redirection messages which are not yet in the cluster state
  @volatile private[this] var temporaryClients = List.empty[RedisNodeClient]
  // ensures that all operations fail fast after client is closed instead of being sent further
  @volatile private[this] var failure = Opt.empty[Throwable]
  private val initPromise = Promise[Unit]
  initPromise.future.foreachNow(_ => initSuccess = true)

  private def ifReady[T](code: => Future[T]): Future[T] = failure match {
    case Opt.Empty if initSuccess => code
    case Opt.Empty => initPromise.future.flatMapNow(_ => code)
    case Opt(t) => Future.failed(t)
  }

  // invoked by monitoring actor, effectively serialized
  private def onNewState(newState: ClusterState): Unit = {
    state = newState
    temporaryClients = Nil
    stateListener(state)
    if (!initSuccess) {
      import system.dispatcher
      Future.traverse(state.masters.values)(_.initialized).toUnit.onComplete { result =>
        // This check handles situation where some node goes down _exactly_ during RedisClusterClient initialization.
        // The failed node was in the first cluster state that we fetched and we tried to connect to it
        // but Redis Cluster detected the failure and cluster state changed. When `RedisMonitoringActor` detects this
        // change, `RedisNodeClient` for the failed node is killed and initialization fails. But we shouldn't fail
        // initialization of entire `RedisClusterClient` because we know that the state changed and thus we should retry.
        // In other words, this check avoids failing entire `RedisClusterClient` based on obsolete cluster state.
        // This scenario is tested by `RedisClusterClientInitDuringFailureTest`.
        if (state eq newState) {
          initPromise.tryComplete(result)
        }
      }
    }
  }

  // invoked by monitoring actor, effectively serialized
  private def onTemporaryClient(client: RedisNodeClient): Unit = {
    temporaryClients = client :: temporaryClients
  }

  private val monitoringActor =
    system.actorOf(Props(new ClusterMonitoringActor(seedNodes, config, initPromise.failure, onNewState, onTemporaryClient)))

  private def determineSlot(pack: RawCommandPack): Int = {
    var slot = -1
    pack.foreachKey { key =>
      val s = Hash.slot(key)
      if (slot == -1) {
        slot = s
      } else if (s != slot) {
        throw new CrossSlotException
      }
    }
    if (slot >= 0) slot
    else throw new NoKeysException
  }

  /**
    * Sets a listener that is notified every time [[RedisClusterClient]] detects a change in cluster state
    * (slot mapping).
    */
  def setStateListener(listener: ClusterState => Unit)(implicit executor: ExecutionContext): Unit =
    stateListener = state => executor.execute(jRunnable(listener(state)))

  /**
    * Returns currently known cluster state.
    */
  def currentState: ClusterState =
    state

  /**
    * Waits until cluster state is known and [[RedisNodeClient]] for every master node is initialized.
    */
  def initialized: Future[this.type] =
    initPromise.future.mapNow(_ => this)

  private def remainingTimeout(startTime: Long, overallTimeout: Timeout): Timeout =
    (overallTimeout.duration - (System.nanoTime() - startTime).nanos) max Duration.Zero

  private def handleRedirection[T](pack: RawCommandPack, slot: Int, result: Future[RedisReply],
    retryCount: Int, startTime: Long, overallTimeout: Timeout): Future[RedisReply] =
    result.flatMapNow {
      case RedirectionReply(red) =>
        retryRedirected(pack, red, retryCount, startTime, overallTimeout)
      case _ => result
    } recoverWithNow {
      case _: NodeRemovedException =>
        // Node went down and we didn't get a regular redirection but the cluster detected the failure
        // and we now have a new cluster view, so retry our not-yet-sent request using new cluster state.
        implicit def timeout: Timeout = remainingTimeout(startTime, overallTimeout)
        val client = state.clientForSlot(slot)
        if (retryCount >= config.maxRedirections)
          throw new TooManyRedirectionsException(Redirection(client.address, slot, ask = false))
        else {
          val result = client.executeRaw(pack).mapNow(_.apply(0))
          handleRedirection(pack, slot, result, retryCount + 1, startTime, overallTimeout)
        }
    }

  private def retryRedirected(pack: RawCommandPack, redirection: Redirection,
    retryCount: Int, startTime: Long, overallTimeout: Timeout): Future[RedisReply] = {

    if (!redirection.ask) {
      // redirection (without ASK) indicates that we may have old cluster state, refresh it
      monitoringActor ! Refresh(new SingletonSeq(redirection.address).opt)
    }
    val packToResend = if (redirection.ask) new AskingPack(pack) else pack
    if (retryCount >= config.maxRedirections)
      Future.successful(FailureReply(new TooManyRedirectionsException(redirection)))
    else {
      implicit def timeout: Timeout = remainingTimeout(startTime, overallTimeout)
      val result = readyClient(redirection.address) match {
        case Opt(client) =>
          client.executeRaw(packToResend)
        case Opt.Empty =>
          // this should only happen when a new master appears and we don't yet have a client for it
          // because we still have old cluster state without that master
          askForClient(redirection.address).flatMapNow(_.executeRaw(packToResend))
      }
      handleRedirection(pack, redirection.slot, result.mapNow(_.apply(0)), retryCount + 1, startTime, overallTimeout)
    }
  }

  private def readyClient(address: NodeAddress): Opt[RedisNodeClient] =
    state.masters.getOpt(address) orElse temporaryClients.findOpt(_.address == address)

  private def askForClient(address: NodeAddress): Future[RedisNodeClient] =
    monitoringActor.ask(GetClient(address))(RedisClusterClient.GetClientTimeout)
      .flatMapNow({ case GetClientResponse(client) => client.initialized })

  /**
    * Returns a [[RedisNodeClient]] internally used to connect to a master with given address.
    * Note that the address may belong to a master that is not yet known to this cluster client.
    * In such case, node client for this master will be created on demand. However, be aware that this temporary
    * client will be immediately closed if it's master is not listed in next cluster state fetched by
    * [[RedisClusterClient]]. Therefore, you can't use this method to obtain clients for arbitrary nodes - only for
    * master nodes that this cluster client knows or is about to know upon next state refresh.
    *
    * This method is primarily intended to be used when having to manually recover from a cluster redirection.
    * If you want to obtain node client serving particular hash slot, get it from [[currentState]].
    */
  def masterClient(address: NodeAddress): Future[RedisNodeClient] = ifReady {
    readyClient(address).fold(askForClient(address))(Future.successful)
  }

  def executionContext: ExecutionContext = system.dispatcher

  /**
    * Executes a [[RedisBatch]] on a Redis Cluster deployment. In order to determine node on which each command must be
    * executed, every command or `MULTI`-`EXEC` transaction in the batch must contain at least one key.
    * Also, in the scope of a single command or transaction all keys must hash to the same slot.
    *
    * However, each command or transaction in the batch may target different slot.
    * [[RedisClusterClient]] automatically splits the original batch and creates smaller batches, one for every master
    * node that needs to be contacted. In other words, commands and transactions from the original batch are
    * automatically distributed over Redis Cluster master nodes, in parallel, using a scatter-gather like manner.
    *
    * [[RedisClusterClient]] also automatically retries execution of commands that fail due to cluster redirections
    * ([[http://redis.io/topics/cluster-spec#moved-redirection MOVED]] and [[http://redis.io/topics/cluster-spec#ask-redirection ASK]])
    * and cluster state changes. However, remember that multi-key commands may still fail with
    * `TRYAGAIN` error during resharding of the slot targeted by the multi-key operation.
    * (see [[http://redis.io/topics/cluster-spec#multiple-keys-operations Redis Cluster specification]]).
    * [[RedisClusterClient]] makes no attempt to recover from these errors. It would require waiting for an
    * indeterminate time until the migration is finished. The maximum number of consecutive retries caused by
    * redirections is configured by [[config.ClusterConfig.maxRedirections maxRedirections]].
    * See [[http://redis.io/topics/cluster-spec#redirection-and-resharding Redis Cluster specification]] for
    * more detailed information on redirections.
    *
    * In general, you can assume that if there are no redirections involved, commands executed on the same master
    * node are executed in the same order as specified in the original batch.
    *
    * Execution of each command in the batch or the whole batch may fail due to following reasons:
    * <ul>
    * <li>[[exception.NoKeysException NoKeysException]] when some command or transaction contains no keys</li>
    * <li>[[exception.CrossSlotException CrossSlotException]] when some command or transaction
    * contains keys hashing to different slots</li>
    * <li>[[exception.ForbiddenCommandException ForbiddenCommandException]] when trying to execute command not
    * supported by this client type</li>
    * <li>[[exception.ErrorReplyException ErrorReplyException]] when Redis server replies with an error for some command</li>
    * <li>[[exception.UnexpectedReplyException UnexpectedReplyException]] when Redis server replies with something
    * unexpected by a decoder of some command</li>
    * <li>[[exception.ConnectionClosedException ConnectionClosedException]] when connection is closed or
    * reset (the client reconnects automatically after connection failure but commands that were in the middle of
    * execution may still fail)</li>
    * <li>[[exception.WriteFailedException WriteFailedException]] when a network write
    * failed</li>
    * <li>[[exception.TooManyRedirectionsException TooManyRedirectionsException]] when
    * a command was replied with `MOVED` or `ASK` redirection too many times in a row. This might indicate
    * misconfiguration of the Redis Cluster deployment.</li>
    * </ul>
    */
  def executeBatch[A](batch: RedisBatch[A])(implicit timeout: Timeout): Future[A] = {
    batch.rawCommandPacks.requireLevel(Level.Node, "ClusterClient")
    ifReady {
      val currentState = state
      currentState.nonClustered match {
        case Opt(client) =>
          client.executeBatch(batch)
        case Opt.Empty =>
          val packs = batch.rawCommandPacks
          val undecodedResult = packs.single match {
            case Opt(pack) => executeSinglePack(pack, currentState)
            case Opt.Empty => executeClusteredPacks(packs, currentState)
          }
          undecodedResult.mapNow(replies => batch.decodeReplies(replies))
      }
    }
  }

  private def executeSinglePack(pack: RawCommandPack, currentState: ClusterState)(implicit timeout: Timeout) = {
    // optimization for single-pack (e.g. single-command) batches that avoids creating unnecessary data structures
    val slot = determineSlot(pack)
    val client = currentState.clientForSlot(slot)
    val result = client.executeRaw(pack).mapNow(_.apply(0))
    handleRedirection(pack, slot, result, 0, System.nanoTime(), timeout).mapNow(PacksResult.Single)
  }

  private def executeClusteredPacks[A](packs: RawCommandPacks, currentState: ClusterState)(implicit timeout: Timeout) = {
    val barrier = Promise[Unit]
    val packsByNode = new mutable.OpenHashMap[RedisNodeClient, ArrayBuffer[RawCommandPack]]
    val resultsByNode = new mutable.OpenHashMap[RedisNodeClient, Future[PacksResult]]

    def futureForPack(slot: Int, client: RedisNodeClient, pack: RawCommandPack): Future[RedisReply] = {
      val packBuffer = packsByNode.getOrElseUpdate(client, new ArrayBuffer)
      val idx = packBuffer.size
      packBuffer += pack
      val result = resultsByNode.getOrElseUpdate(client,
        barrier.future.flatMapNow(_ => client.executeRaw(CollectionPacks(packBuffer)))
      ).mapNow(_.apply(idx))
      handleRedirection(pack, slot, result, 0, System.nanoTime(), timeout)
    }

    val results = new ArrayBuffer[Future[RedisReply]]
    packs.emitCommandPacks { pack =>
      val resultFuture = try {
        val slot = determineSlot(pack)
        val client = currentState.clientForSlot(slot)
        futureForPack(slot, client, pack)
      } catch {
        case re: RedisException => Future.successful(FailureReply(re))
        case NonFatal(cause) => Future.failed(cause)
      }
      results += resultFuture
    }
    barrier.success(())
    implicit val ec: ExecutionContext = RunInQueueEC
    Future.sequence(results)
  }

  def close(): Unit = {
    failure = new ClientStoppedException(Opt.Empty).opt
    system.stop(monitoringActor)
  }
}

private object RedisClusterClient {
  val GetClientTimeout = Timeout(1.seconds)

  case class CollectionPacks(coll: Traversable[RawCommandPack]) extends RawCommandPacks {
    def emitCommandPacks(consumer: RawCommandPack => Unit) = coll.foreach(consumer)
  }

  final class AskingPack(pack: RawCommandPack) extends RawCommandPack {
    private val keyed = {
      var result = false
      pack.rawCommands(inTransaction = true).emitCommands { cmd =>
        result = result || cmd.encoded.elements.exists(_.isCommandKey)
      }
      result
    }

    override def isAsking = true

    def rawCommands(inTransaction: Boolean) =
      if (inTransaction || pack.isAsking || !keyed) pack.rawCommands(inTransaction)
      else new RawCommands {
        def emitCommands(consumer: RawCommand => Unit) = {
          consumer(Asking)
          pack.rawCommands(inTransaction).emitCommands(consumer)
        }
      }

    def createPreprocessor(replyCount: Int) =
      if (pack.isAsking || !keyed) pack.createPreprocessor(replyCount)
      else new ReplyPreprocessor {
        private val wrapped = pack.createPreprocessor(replyCount - 1)
        private var first = true
        private var error: Opt[FailureReply] = Opt.Empty

        def preprocess(message: RedisMsg, watchState: WatchState) =
          if (first) {
            first = false
            message match {
              case RedisMsg.Ok =>
              case _ => error = FailureReply(new UnexpectedReplyException(s"Unexpected reply for ASKING: $message")).opt
            }
            Opt.Empty
          } else wrapped.preprocess(message, watchState)
            .map(reply => error.getOrElse(reply))
      }

    def checkLevel(minAllowed: Level, clientType: String) =
      pack.checkLevel(minAllowed, clientType)
  }
}

object RedirectionReply {
  def unapply(reply: RedisReply): Opt[Redirection] = reply match {
    case RedirectionError(redirection) =>
      redirection.opt
    case TransactionReply(elements) =>
      def collectRedirection(acc: Opt[Redirection], idx: Int): Opt[Redirection] =
        if (idx >= elements.size) acc
        else elements(idx) match {
          case RedirectionError(redirection) if acc.forall(_ == redirection) =>
            collectRedirection(redirection.opt, idx + 1)
          case err: ErrorMsg if err.errorCode == "EXECABORT" =>
            collectRedirection(acc, idx + 1)
          case _ => Opt.Empty
        }
      collectRedirection(Opt.Empty, 0)
    case _ => Opt.Empty
  }
}

object RedirectionError {
  def unapply(failure: ErrorMsg): Opt[Redirection] = {
    val message = failure.errorString.utf8String
    val moved = message.startsWith("MOVED ")
    val ask = message.startsWith("ASK ")
    if (moved || ask) {
      val Array(_, slot, ipport) = message.split(' ')
      val Array(ip, port) = ipport.split(':')
      Opt(Redirection(NodeAddress(ip, port.toInt), slot.toInt, ask))
    } else Opt.Empty
  }
}

object RedirectionException {
  def unapply(exception: ErrorReplyException): Opt[Redirection] =
    RedirectionError.unapply(exception.reply)
}

case class Redirection(address: NodeAddress, slot: Int, ask: Boolean)

/**
  * Current cluster state known by [[RedisClusterClient]].
  *
  * @param mapping      mapping between slot ranges and node clients that serve them, sorted by slot ranges
  * @param masters      direct mapping between master addresses and node clients
  * @param nonClustered non-empty if there's actually only one, non-clustered Redis node - see
  *                     [[com.avsystem.commons.redis.config.ClusterConfig.fallbackToSingleNode fallbackToSingleNode]]
  */
case class ClusterState(
  mapping: IndexedSeq[(SlotRange, RedisNodeClient)],
  masters: BMap[NodeAddress, RedisNodeClient],
  nonClustered: Opt[RedisNodeClient] = Opt.Empty
) {

  nonClustered.foreach { client =>
    require(!client.clusterNode && mapping == IndexedSeq(SlotRange.Full -> client) && masters == Map(client.address -> client))
  }

  /**
    * Obtains a [[RedisNodeClient]] that currently serves particular hash slot. This is primarily used to
    * execute `WATCH`-`MULTI`-`EXEC` transactions on a Redis Cluster deployment ([[RedisClusterClient]] cannot
    * directly execute them). For example:
    *
    * {{{
    *   import scala.concurrent.duration._
    *   implicit val actorSystem: ActorSystem = ActorSystem()
    *   implicit val timeout: Timeout = 10.seconds
    *   val clusterClient = new RedisClusterClient
    *   import RedisApi.Batches.StringTyped._
    *   import scala.concurrent.ExecutionContext.Implicits._
    *
    *   // transactionally divide number stored under "key" by 3
    *   val transaction: RedisOp[Unit] = for {
    *     // this causes WATCH and GET to be sent in a single batch
    *     value <- watch("key") *> get("key").map(_.fold(0)(_.toInt))
    *     // this causes SET wrapped in a MULTI-EXEC block to be sent
    *     _ <- set("key", (value / 3).toString).transaction
    *   } yield ()
    *
    *   val executedTransaction: Future[Unit] =
    *     clusterClient.initialized.map(_.currentState).flatMap { state =>
    *       state.clientForSlot(keySlot("key")).executeOp(transaction)
    *     }
    * }}}
    *
    * However, be aware that when executing transactions on node clients obtained from [[ClusterState]], you must
    * manually handle cluster redirections and cluster state changes
    * ([[exception.NodeRemovedException NodeRemovedException]])
    */
  def clientForSlot(slot: Int): RedisNodeClient = {
    def binsearch(from: Int, to: Int): RedisNodeClient =
      if (from >= to) throw new UnmappedSlotException(slot)
      else {
        val mid = (from + to) / 2
        val (range, client) = mapping(mid)
        if (range.contains(slot)) client
        else if (range.start > slot) binsearch(from, mid)
        else binsearch(mid + 1, to)
      }
    binsearch(0, mapping.length)
  }
}
object ClusterState {
  val Empty = ClusterState(IndexedSeq.empty, BMap.empty)
  def nonClustered(client: RedisNodeClient): ClusterState =
    ClusterState(IndexedSeq(SlotRange.Full -> client), Map(client.address -> client), client.opt)
}
