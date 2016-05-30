package com.avsystem.rpc.akka

import akka.actor.Actor
import monifu.concurrent.Scheduler
import monifu.reactive.{Ack, Observer}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * @author Wojciech Milewski
  */
class ServerActor(rawRPC: AkkaRPCFramework.RawRPC) extends Actor {
  implicit val scheduler = Scheduler(context.dispatcher)

  override def receive: Receive = {
    case Request.UnitRpcCallMessage(name, paramLists) => rawRPC.fire(name, paramLists)
    case Request.FutureRpcCallMessage(id, name, paramLists) =>
      val respondToActor = sender()
      rawRPC.call(name, paramLists).onComplete {
        case Success(value) => respondToActor ! Response.SuccessfulResponseMessage(id, value)
        case Failure(exception) => respondToActor ! Response.FailureResponseMessage(id, exception.getClass.getCanonicalName)
      }
    case Request.ObservableRpcCallMessage(id, name, paramLists) =>
      val respondToActor = sender()
      rawRPC.observe(name, paramLists).subscribe(new Observer[Any] {
        override def onError(ex: Throwable): Unit = respondToActor ! Response.Observable.FailureResponseMessage(id, ex.getClass.getCanonicalName)
        override def onComplete(): Unit = respondToActor ! Response.Observable.CompleteResponseMessage(id)
        override def onNext(t: Any): Future[Ack] = {
          respondToActor ! Response.Observable.NextResponseMessage(id, t)
          Ack.Continue
        }
      })
  }
}

case class RpcCallId(value: String)

object Request {
  case class UnitRpcCallMessage(name: String, paramLists: List[List[Any]])
  case class FutureRpcCallMessage(callId: RpcCallId, name: String, paramLists: List[List[Any]])
  case class ObservableRpcCallMessage(callId: RpcCallId, name: String, paramLists: List[List[Any]])
}

object Response {
  case class SuccessfulResponseMessage(callId: RpcCallId, value: Any)
  case class FailureResponseMessage(callId: RpcCallId, exceptionName: String)

  object Observable {
    case class NextResponseMessage(callId: RpcCallId, value: Any)
    case class CompleteResponseMessage(callId: RpcCallId)
    case class FailureResponseMessage(callId: RpcCallId, exceptionName: String)
  }
}