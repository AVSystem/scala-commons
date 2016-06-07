package com.avsystem.commons
package rpc.akka.server

import akka.actor.{Actor, ActorLogging, Props}
import com.avsystem.commons.rpc.akka.{AkkaRPCFramework, FunctionInvocationMessage, InvocationMessage, ObservableCompleteMessage, ObservableInvocationMessage, ProcedureInvocationMessage, RemoteCallException}
import monifu.concurrent.Scheduler
import monifu.reactive.{Ack, Observer}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * @author Wojciech Milewski
  */
class ServerActor(rawRPC: AkkaRPCFramework.RawRPC) extends Actor with ActorLogging {

  override def receive: Receive = {
    case msg @ ProcedureInvocationMessage(name, argLists, getterChain) =>
      resolveRpc(msg).fire(name, argLists)
    case msg @ FunctionInvocationMessage(name, argLists, getterChain) =>
      import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext
      val s = sender()
      resolveRpc(msg).call(name, argLists).onComplete {
        case Success(value) => s ! value
        case Failure(e) => s ! new RemoteCallException(e.getClass.getCanonicalName)
      }
    case msg @ ObservableInvocationMessage(name, argLists, getterChain) =>
      implicit val scheduler = Scheduler(context.dispatcher)
      val s = sender()
      resolveRpc(msg).observe(name, argLists).subscribe(new Observer[AkkaRPCFramework.RawValue] {
        override def onError(ex: Throwable): Unit = s ! new RemoteCallException(ex.getClass.getCanonicalName)
        override def onComplete(): Unit = s ! ObservableCompleteMessage
        override def onNext(elem: AkkaRPCFramework.RawValue): Future[Ack] = {
          s ! elem
          Ack.Continue
        }
      })
  }

  private def resolveRpc(msg: InvocationMessage) = rawRPC.resolveGetterChain(msg.getterChain.map(r => AkkaRPCFramework.RawInvocation(r.rpcName, r.argLists)).toList)
}

object ServerActor {
  def props(rawRPC: AkkaRPCFramework.RawRPC): Props = Props(new ServerActor(rawRPC))
}