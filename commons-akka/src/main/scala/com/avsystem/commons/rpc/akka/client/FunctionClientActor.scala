package com.avsystem.commons
package rpc.akka.client

import akka.actor.{Actor, ActorPath, Props, Status}
import akka.pattern.{AskTimeoutException, ask, pipe}
import akka.util.Timeout
import com.avsystem.commons.rpc.akka.{AkkaRPCClientConfig, AkkaRPCFramework, FunctionInvocationMessage, RemoteCallException, RemoteTimeoutException}

import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * @author Wojciech Milewski
  */
private[akka] class FunctionClientActor(config: AkkaRPCClientConfig) extends Actor {

  implicit val timeout = Timeout(config.functionCallTimeout)

  override def receive: Receive = {
    case msg: FunctionInvocationMessage =>
      val s = sender()
      val response = context.actorSelection(config.serverPath) ? msg
      import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext

      response.onComplete {
        case Success(exception: RemoteCallException) => s ! Status.Failure(exception)
        case Success(value: AkkaRPCFramework.RawValue) => s ! value
        case Success(_) => //well, ignore? todo: redesign sent messages including proper serialization
        case Failure(e: AskTimeoutException) => s ! Status.Failure(RemoteTimeoutException)
        case Failure(e) => s ! Status.Failure(e)
      }
  }
}

private[akka] object FunctionClientActor {
  def props(config: AkkaRPCClientConfig): Props = Props(new FunctionClientActor(config))
}
