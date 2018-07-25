package com.avsystem.commons
package rpc.akka.server

import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.rpc.akka.AkkaRPCFramework._
import com.avsystem.commons.rpc.akka._
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable

/**
  * @author Wojciech Milewski
  */
private final class ServerActor(rawRPC: RawRPC, config: AkkaRPCServerConfig) extends Actor with ActorLogging {

  override def receive: Receive = {
    case ProcedureInvocationMessage(invocation, getterChain) =>
      resolveRpc(getterChain).fire(invocation)
    case FunctionInvocationMessage(invocation, getterChain) =>
      val s = sender()
      resolveRpc(getterChain).call(invocation).onCompleteNow {
        case Success(value) => s ! InvocationSuccess(value)
        case Failure(e) =>
          logError(e, invocation.rpcName)
          s ! InvocationFailure(e.getClass.getCanonicalName, e.getMessage)
      }
    case ObservableInvocationMessage(invocation, getterChain) =>
      implicit val scheduler: Scheduler = Scheduler(RunNowEC)
      implicit val timeout: Timeout = Timeout(config.observableAckTimeout)
      val s = sender()

      val heartbeat = Observable.timerRepeated(config.heartbeatInterval, config.heartbeatInterval, MonixProtocol.Heartbeat)
        .subscribe { beat =>
          s ! beat
          Ack.Continue
        }

      resolveRpc(getterChain).observe(invocation).subscribe(
        value => {
          val result = s ? InvocationSuccess(value)
          result.mapTo[MonixProtocol.RemoteAck].map {
            case MonixProtocol.Continue => Ack.Continue
            case MonixProtocol.Stop =>
              heartbeat.cancel()
              Ack.Stop
          }.recover {
            case e: AskTimeoutException =>
              heartbeat.cancel()
              log.error(e, "Client actor didn't respond within requested time.")
              Ack.Stop
          }
        },
        e => {
          heartbeat.cancel()
          logError(e, invocation.rpcName)
          s ! InvocationFailure(e.getClass.getCanonicalName, e.getMessage)
        },
        () => {
          heartbeat.cancel()
          s ! MonixProtocol.StreamCompleted
        }
      )
  }

  private def resolveRpc(getterChain: Seq[RawInvocation]): RawRPC =
    rawRPC.resolveGetterChain(getterChain)

  private def logError(e: Throwable, methodName: String): Unit = {
    log.error(e,
      """
        |Server exception. Remote method called: {}.
        |Original message: {}.
      """.stripMargin, methodName, e.getMessage)
  }
}

private[akka] object ServerActor {
  def props(rawRPC: RawRPC, config: AkkaRPCServerConfig): Props = Props(new ServerActor(rawRPC, config))
}
