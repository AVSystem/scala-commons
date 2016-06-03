package com.avsystem.commons
package rpc.akka.client

import akka.actor.{ActorPath, ActorRef}
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.Timeout
import com.avsystem.commons.rpc.akka.{AkkaRPCFramework, FunctionInvocationMessage, ObservableInvocationMessage, ProcedureInvocationMessage}
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawRPC
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawInvocation
import monifu.reactive.Observable
import org.reactivestreams.Publisher

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
private[akka] class ClientRawRPC(procedureActor: ActorRef, functionActor: ActorRef, serverPath: ActorPath, getterChain: Seq[RawInvocation] = Nil)(implicit materializer: ActorMaterializer) extends AkkaRPCFramework.RawRPC {
  implicit val timeout = Timeout(30.seconds)    //todo make it configurable

  override def fire(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Unit = procedureActor ! ProcedureInvocationMessage(rpcName, argLists, getterChain)
  override def call(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Future[AkkaRPCFramework.RawValue] = {
    val future = functionActor ? FunctionInvocationMessage(rpcName, argLists, getterChain)
    future.mapTo[AkkaRPCFramework.RawValue]
  }
  override def get(rpcName: String, argLists: List[List[Any]]): RawRPC =
    new ClientRawRPC(procedureActor, functionActor, serverPath, getterChain :+ RawInvocation(rpcName, argLists))

  override def observe(rpcName: String, argLists: List[List[AkkaRPCFramework.RawValue]]): Observable[AkkaRPCFramework.RawValue] = {
    val publisher: Publisher[AkkaRPCFramework.RawValue] = Source.actorPublisher(ObservableClientActor.props(
      serverPath, ObservableInvocationMessage(rpcName, argLists, getterChain)
    )).runWith(Sink.asPublisher(fanout = true))
    Observable.fromReactivePublisher(publisher)
  }
}
