package com.avsystem.commons
package rpc.akka.client

import akka.actor.{Actor, Props, ReceiveTimeout}
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawValue
import com.avsystem.commons.rpc.akka.{AkkaRPCClientConfig, InvocationFailure, InvocationSuccess, MonifuProtocol, ObservableInvocationMessage, RemoteCallException, RemoteTimeoutException}
import monifu.reactive.{Ack, Subscriber}

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

/**
  * @author Wojciech Milewski
  */
private final class MonifuClientActor(subscriber: Subscriber[RawValue], config: AkkaRPCClientConfig) extends Actor {

  override def receive: Receive = initializing

  private def initializing: Receive = {
    case m: ObservableInvocationMessage =>
      context.actorSelection(config.serverPath) ! m
      context become active
      context setReceiveTimeout config.observableMessageTimeout
  }

  private def active: Receive = {
    case InvocationSuccess(value) =>
      //todo is it really working well?
      context setReceiveTimeout Duration.Undefined
      import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext
      val s = sender()
      subscriber.onNext(value).onComplete {
        case Success(Ack.Continue) =>
          s ! MonifuProtocol.Continue
          context setReceiveTimeout config.observableMessageTimeout
        case Success(Ack.Cancel) =>
          s ! MonifuProtocol.Cancel
          context stop self
        case Failure(e) =>
          subscriber.onError(e)
          s ! MonifuProtocol.Cancel
          context stop self
      }
    case InvocationFailure(name, message) =>
      subscriber.onError(new RemoteCallException(name, message))
      context stop self
    case MonifuProtocol.StreamCompleted =>
      subscriber.onComplete()
      context stop self
    case ReceiveTimeout =>
      subscriber.onError(RemoteTimeoutException)
      context stop self
  }
}

private object MonifuClientActor {
  def props(s: Subscriber[RawValue], config: AkkaRPCClientConfig) = Props(new MonifuClientActor(s, config))
}