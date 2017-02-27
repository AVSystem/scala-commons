package com.avsystem.commons
package rpc.akka.client

import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout, Status}
import akka.pattern.pipe
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawValue
import com.avsystem.commons.rpc.akka.{AkkaRPCClientConfig, InvocationFailure, InvocationSuccess, MonixProtocol, ObservableInvocationMessage, RemoteCallException, RemoteTimeoutException}
import monix.execution.Ack
import monix.reactive.observers.Subscriber

import scala.concurrent.duration.Duration

/**
  * @author Wojciech Milewski
  */
private final class MonixClientActor(subscriber: Subscriber[RawValue], config: AkkaRPCClientConfig) extends Actor {

  override def receive: Receive = initializing

  private def initializing: Receive = {
    case m: ObservableInvocationMessage =>
      context.actorSelection(config.serverPath) ! m
      context.become(waitingForServer)
      context.setReceiveTimeout(config.observableMessageTimeout)
  }

  private def waitingForServer: Receive = {
    case MonixProtocol.Heartbeat =>
    //do absolutely nothing as this message should only reset timeout counter
    case InvocationSuccess(value) =>
      context.setReceiveTimeout(Duration.Undefined)
      import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext
      val s = sender()
      subscriber.onNext(value).pipeTo(self)
      context.become(waitingForConsumer(s))
    case InvocationFailure(name, message) =>
      subscriber.onError(new RemoteCallException(name, message))
      context.stop(self)
    case MonixProtocol.StreamCompleted =>
      subscriber.onComplete()
      context.stop(self)
    case ReceiveTimeout =>
      subscriber.onError(RemoteTimeoutException)
      context.stop(self)
  }

  private def waitingForConsumer(server: ActorRef): Receive = {
    case Ack.Continue =>
      server ! MonixProtocol.Continue
      context.setReceiveTimeout(config.observableMessageTimeout)
      context.become(waitingForServer)
    case Ack.Stop =>
      server ! MonixProtocol.Stop
      context.stop(self)
    case Status.Failure(cause) =>
      subscriber.onError(cause)
      server ! MonixProtocol.Stop
      context.stop(self)
  }
}

private object MonixClientActor {
  def props(s: Subscriber[RawValue], config: AkkaRPCClientConfig) = Props(new MonixClientActor(s, config))
}