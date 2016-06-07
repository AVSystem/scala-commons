package com.avsystem.commons
package rpc.akka.client

import akka.actor.ActorRef
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.Timeout
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawRPC
import com.avsystem.commons.rpc.akka.{AkkaRPCClientConfig, AkkaRPCFramework, FunctionInvocationMessage, ObservableInvocationMessage, ProcedureInvocationMessage, RawInvocation}
import monifu.reactive.Observable
import org.reactivestreams.Publisher

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
private[akka] class ClientRawRPC(procedureActor: ActorRef, functionActor: ActorRef, config: AkkaRPCClientConfig, getterChain: Seq[RawInvocation] = Nil)(implicit materializer: ActorMaterializer) extends AkkaRPCFramework.RawRPC {
  /**
    * Function actor should always respond within config.functionCallTimeout,
    * but sending message from functionActor to this object take some time.
    */
  private implicit val timeout = Timeout(config.functionCallTimeout.plus(1.second))

  override def fire(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Unit = procedureActor ! ProcedureInvocationMessage(rpcName, argLists, getterChain)
  override def call(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Future[AkkaRPCFramework.RawValue] = {
    val future = functionActor ? FunctionInvocationMessage(rpcName, argLists, getterChain)
    future.mapTo[AkkaRPCFramework.RawValue]
  }
  override def get(rpcName: String, argLists: List[List[Any]]): RawRPC =
    new ClientRawRPC(procedureActor, functionActor, config, getterChain :+ RawInvocation(rpcName, argLists))

  override def observe(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Observable[AkkaRPCFramework.RawValue] = {
    val publisher: Publisher[AkkaRPCFramework.RawValue] = Source.actorPublisher(ObservableClientActor.props(
      config, ObservableInvocationMessage(rpcName, argLists, getterChain)
    )).runWith(Sink.asPublisher(fanout = true))
    Observable.fromReactivePublisher(publisher)
  }
}
