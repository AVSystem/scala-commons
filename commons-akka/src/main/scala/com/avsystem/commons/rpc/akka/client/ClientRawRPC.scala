package com.avsystem.commons
package rpc.akka.client

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawRPC
import com.avsystem.commons.rpc.akka._
import monix.execution.Cancelable
import monix.reactive.{Observable, OverflowStrategy}

/**
  * @author Wojciech Milewski
  */
private[akka] final class ClientRawRPC(config: AkkaRPCClientConfig, getterChain: Seq[RawInvocation] = Nil)(implicit system: ActorSystem) extends AkkaRPCFramework.RawRPC {

  override def fire(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Unit = {
    system.actorSelection(config.serverPath) ! ProcedureInvocationMessage(rpcName, argLists, getterChain)
  }
  override def call(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Future[AkkaRPCFramework.RawValue] = {
    implicit val timeout = Timeout(config.functionCallTimeout)
    val future = system.actorSelection(config.serverPath) ? FunctionInvocationMessage(rpcName, argLists, getterChain)

    import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext

    future.flatMap {
      case InvocationSuccess(value) => Future.successful(value)
      case InvocationFailure(name, message) => Future.failed(new RemoteCallException(name, message))
      case value => Future.failed(new IllegalStateException(s"Illegal message type. Should be InvocationResult, but received value was: $value"))
    }
  }
  override def get(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): RawRPC =
    new ClientRawRPC(config, getterChain :+ RawInvocation(rpcName, argLists))

  override def observe(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Observable[AkkaRPCFramework.RawValue] = {
    Observable.create(OverflowStrategy.Unbounded) { s =>
      val actor = system.actorOf(MonixClientActor.props(s, config))
      actor ! ObservableInvocationMessage(rpcName, argLists, getterChain)
      Cancelable.empty // TODO implement proper canceling
    }

  }
}
