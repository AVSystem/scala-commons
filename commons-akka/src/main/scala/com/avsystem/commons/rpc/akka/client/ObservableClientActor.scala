package com.avsystem.commons
package rpc.akka.client

import com.avsystem.commons.collection.CollectionAliases._
import akka.actor.{Actor, ActorLogging, ActorPath, FSM, Props}
import akka.stream.actor.ActorPublisher
import com.avsystem.commons.rpc.akka.{AkkaRPCFramework, ObservableCompleteMessage, ObservableInvocationMessage, RemoteCallException, RemoteTimeoutException}

import scala.annotation.tailrec
import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
private[akka] final class ObservableClientActor(serverActorPath: ActorPath, invocationMessage: ObservableInvocationMessage)
  extends ActorPublisher[AkkaRPCFramework.RawValue] with FSM[State, Data] with ActorLogging {

  import ObservableClientActor._
  import akka.stream.actor.ActorPublisherMessage._

  val senderActor = context.actorOf(SenderActor.props(serverActorPath))
  val timeout = 1.second    //todo read from config

  var buf = Vector.empty[AkkaRPCFramework.RawValue]
  var streamClosed = false

  override def preStart(): Unit = {
    super.preStart()
    senderActor ! invocationMessage
  }

  startWith(OpenedStream, Buffer(), Some(timeout))

  when(OpenedStream, stateTimeout = timeout) {
    case Event(FSM.StateTimeout, b: Buffer) =>
      val e = RemoteTimeoutException
      val rest = deliverWithError(e, b.values)
      goto(FailedStream) using Error(e, rest)
    case Event(Request(_), b: Buffer) =>
      val rest = deliver(b.values)
      stay using Buffer(rest)
    case Event(e: RemoteCallException, b: Buffer) =>
      val rest = deliverWithError(e, b.values)
      goto(FailedStream) using Error(e, rest)
    case Event(ObservableCompleteMessage, b: Buffer) =>
      val rest = deliverWithComplete(b.values)
      goto(FinishedStream) using Buffer(rest)
    case Event(msg: AkkaRPCFramework.RawValue, b: Buffer) if b.values.size == BufferLimit => stay
    case Event(msg: AkkaRPCFramework.RawValue, b: Buffer) =>
      val rest = deliver(b.values :+ msg)
      stay using Buffer(rest)
  }

  when(FailedStream) {
    case Event(Request(_), e: Error) =>
      val rest = deliverWithError(e.exception, e.values)
      stay using e.copy(values = rest)
  }

  when(FinishedStream) {
    case Event(Request(_), b: Buffer) =>
      val rest = deliverWithComplete(b.values)
      stay using Buffer(rest)
  }

  whenUnhandled {
    case Event(Cancel, _) =>
      context.stop(self)
      stay
  }

  initialize()

  private def deliver(values: IQueue[AkkaRPCFramework.RawValue], actionIfDeliveredAll: => Any = ()): IQueue[AkkaRPCFramework.RawValue] = {
    @tailrec
    def deliverRecursive(currentBuffer: IQueue[AkkaRPCFramework.RawValue]): IQueue[AkkaRPCFramework.RawValue] = {
      if (totalDemand == 0) currentBuffer
      else if (totalDemand <= Int.MaxValue) {
        val (use, keep) = currentBuffer.splitAt(totalDemand.toInt)
        use foreach onNext
        if (keep.isEmpty) {
          actionIfDeliveredAll
        }
        keep
      } else {
        val (use, keep) = currentBuffer.splitAt(totalDemand.toInt)
        use foreach onNext
        deliverRecursive(keep)
      }
    }
    deliverRecursive(values)
  }
  private def deliverWithComplete(values: IQueue[AkkaRPCFramework.RawValue]): IQueue[AkkaRPCFramework.RawValue] = deliver(values, onCompleteThenStop())
  private def deliverWithError(e: Exception, values: IQueue[AkkaRPCFramework.RawValue]): IQueue[AkkaRPCFramework.RawValue] = deliver(values, onErrorThenStop(e))

}

private[akka] sealed trait State
private case object OpenedStream extends State
private case object FinishedStream extends State
private case object FailedStream extends State

private[akka] sealed trait Data
private final case class Buffer(values: IQueue[AkkaRPCFramework.RawValue] = IQueue.empty) extends Data
private final case class Error(exception: Exception, values: IQueue[AkkaRPCFramework.RawValue]) extends Data

private[akka] object ObservableClientActor {
  def props(serverActorPath: ActorPath, invocationMessage: ObservableInvocationMessage): Props = Props(new ObservableClientActor(serverActorPath, invocationMessage))

  private val BufferLimit = 1000

  private final class SenderActor(serverActorPath: ActorPath) extends Actor {
    override def receive: Receive = {
      case msg: ObservableInvocationMessage => context.actorSelection(serverActorPath) ! msg
      case msg => context.parent ! msg
    }
  }
  private object SenderActor {
    def props(serverPath: ActorPath) = Props(new SenderActor(serverPath))
  }
}