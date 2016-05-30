package com.avsystem.rpc.akka

import java.util.UUID

import akka.actor.{Actor, Props}
import akka.stream.actor.ActorPublisher

import scala.annotation.tailrec

/**
  * @author Wojciech Milewski
  */
class ClientActorPublisher(url: String, callMessage: RpcCallMessage) extends ActorPublisher[Any] {
  import akka.stream.actor.ActorPublisherMessage.{Request => StreamRequest, _}

  val MaxSize = 10000
  private var buffer = Vector.empty[Any]

  override def preStart(): Unit = {
    super.preStart()
    val senderActor = context.actorOf(SenderActor.props(url))
    senderActor ! callMessage
  }
  override def receive: Receive = {
    case Response.Observable.NextResponseMessage(_, value) if buffer.size == MaxSize => //ignore
    case Response.Observable.NextResponseMessage(_, value) =>
      if (buffer.isEmpty && totalDemand > 0) {
        onNext(value)
      } else {
        buffer :+= value
        deliverValues()
      }
    case Response.Observable.CompleteResponseMessage(_) =>
      onCompleteThenStop()
    case StreamRequest(_) =>
      deliverValues()
    case Cancel =>
      context.stop(self)
  }

  @tailrec
  private def deliverValues(): Unit = {
    if (totalDemand > 0) {
      /*
       * totalDemand is a Long and could be larger than
       * what buf.splitAt can accept
       */
      if (totalDemand <= Int.MaxValue) {
        val (use, keep) = buffer.splitAt(totalDemand.toInt)
        buffer = keep
        use foreach onNext
      } else {
        val (use, keep) = buffer.splitAt(Int.MaxValue)
        buffer = keep
        use foreach onNext
        deliverValues()
      }
    }
  }

}

class SenderActor(url: String) extends Actor {
  private val callId = RpcCallId(UUID.randomUUID().toString)

  override def receive: Receive = {
    case callMsg: RpcCallMessage =>
      val serverActor = context.actorSelection(url)
      serverActor ! Request.ObservableRpcCallMessage(callId, callMsg.name, callMsg.paramLists)
    case response => context.parent ! response
  }
}

object SenderActor {
  def props(url: String) = Props(new SenderActor(url))
}