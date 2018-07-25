package com.avsystem.commons
package rpc.akka.client

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.rpc.akka.AkkaRPCFramework._
import com.avsystem.commons.rpc.akka._
import monix.execution.Cancelable
import monix.reactive.{Observable, OverflowStrategy}

/**
  * @author Wojciech Milewski
  */
private[akka] final class ClientRawRPC(config: AkkaRPCClientConfig, getterChain: Seq[RawInvocation] = Nil)(implicit system: ActorSystem) extends AkkaRPCFramework.RawRPC {

  override def fire(invocation: RawInvocation): Unit = {
    system.actorSelection(config.serverPath) ! ProcedureInvocationMessage(invocation, getterChain)
  }
  override def call(invocation: RawInvocation): Future[RawValue] = {
    implicit val timeout: Timeout = Timeout(config.functionCallTimeout)
    val future = system.actorSelection(config.serverPath) ? FunctionInvocationMessage(invocation, getterChain)

    import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext

    future.flatMap {
      case InvocationSuccess(value) => Future.successful(value)
      case InvocationFailure(name, message) => Future.failed(new RemoteCallException(name, message))
      case value => Future.failed(new IllegalStateException(s"Illegal message type. Should be InvocationResult, but received value was: $value"))
    }
  }
  override def get(invocation: RawInvocation): RawRPC =
    new ClientRawRPC(config, getterChain :+ invocation)

  override def observe(invocation: RawInvocation): Observable[RawValue] = {
    Observable.create(OverflowStrategy.Unbounded) { s =>
      val actor = system.actorOf(MonixClientActor.props(s, config))
      actor ! ObservableInvocationMessage(invocation, getterChain)
      Cancelable.empty // TODO implement proper canceling
    }

  }
}
