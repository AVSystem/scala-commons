package com.avsystem.commons
package rpc.akka.client

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.Timeout
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawRPC
import com.avsystem.commons.rpc.akka.{AkkaRPCClientConfig, AkkaRPCFramework, FunctionInvocationMessage, InvocationFailure, InvocationSuccess, ObservableInvocationMessage, ProcedureInvocationMessage, RawInvocation, RemoteCallException}
import monifu.reactive.Observable
import org.reactivestreams.Publisher

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
private[akka] class ClientRawRPC(config: AkkaRPCClientConfig, getterChain: Seq[RawInvocation] = Nil)(implicit system: ActorSystem, materializer: ActorMaterializer) extends AkkaRPCFramework.RawRPC {

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
    val publisher: Publisher[AkkaRPCFramework.RawValue] = Source.actorPublisher(ObservableClientActor.props(
      config, ObservableInvocationMessage(rpcName, argLists, getterChain)
    )).runWith(Sink.asPublisher(fanout = true))
    Observable.fromReactivePublisher(publisher)
  }
}
