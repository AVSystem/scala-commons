package com.avsystem.rpc.akka

import java.util.UUID

import akka.actor.Actor
import com.avsystem.commons.collection.CollectionAliases._
import org.reactivestreams.Subscriber

import scala.concurrent.Promise

/**
  * @author Wojciech Milewski
  */
class ClientActor(url: String) extends Actor {
  import ClientActor._

  private val pendingCalls: MMap[RpcCallId, Promise[Any]] = MMap()

  override def receive: Receive = {
    case UnitRpcCallMessage(msg) =>
      val serverActor = context.actorSelection(url)
      serverActor ! Request.UnitRpcCallMessage(msg.name, msg.paramLists)
    case FutureRpcCallMessage(msg, promise) =>
      val callId = RpcCallId(UUID.randomUUID.toString)
      pendingCalls += callId -> promise
      val serverActor = context.actorSelection(url)
      serverActor ! Request.FutureRpcCallMessage(callId, msg.name, msg.paramLists)
    case Response.SuccessfulResponseMessage(callId, value) =>
      pendingCalls.get(callId).foreach(_.success(value))
      pendingCalls -= callId
    case Response.FailureResponseMessage(callId, exceptionName) =>
      pendingCalls.get(callId).foreach(_.failure(new RemoteCallException(exceptionName)))
      pendingCalls -= callId
  }
}

object ClientActor {
  case class SubscriberMsg(callId: RpcCallId, subscriber: Subscriber[String])

  case class UnitRpcCallMessage(msg: RpcCallMessage)
  case class FutureRpcCallMessage(msg: RpcCallMessage, promise: Promise[Any])
}