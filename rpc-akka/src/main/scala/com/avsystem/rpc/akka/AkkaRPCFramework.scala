package com.avsystem.rpc.akka

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import com.avsystem.commons.rpc.{FunctionRPCFramework, MonifuRPCFramework, ProcedureRPCFramework}
import monifu.reactive.Observable

import scala.concurrent.{Future, Promise}

/**
  * @author Wojciech Milewski
  */
object AkkaRPCFramework extends FunctionRPCFramework with ProcedureRPCFramework with MonifuRPCFramework {
  trait RawRPC extends FunctionRawRPC with ProcedureRawRPC with MonifuRawRPC

  override type RawValue = Any
  override type Reader[T] = DummyImplicit
  override type Writer[T] = DummyImplicit

  override def read[T: Reader](raw: Any): T = raw.asInstanceOf[T]
  override def write[T: Writer](value: T): Any = value

  private[akka] class ClientRPC(url: String)(implicit system: ActorSystem) extends RawRPC {
    val clientActor = system.actorOf(Props(new ClientActor(url)))
    implicit val materializer = ActorMaterializer()

    override def fire(rpcName: String, argLists: List[List[RawValue]]): Unit = {
      clientActor ! ClientActor.UnitRpcCallMessage(RpcCallMessage(rpcName, argLists))
    }
    override def call(rpcName: String, argLists: List[List[RawValue]]): Future[RawValue] = {
      val p = Promise[RawValue]()
      clientActor ! ClientActor.FutureRpcCallMessage(RpcCallMessage(rpcName, argLists), p)
      p.future
    }

    override def observe(rpcName: String, argLists: List[List[RawValue]]): Observable[RawValue] = {
      val pub = Source.actorPublisher(Props(new ClientActorPublisher(url, RpcCallMessage(rpcName, argLists)))).runWith(Sink.asPublisher(fanout = false))
      Observable.fromReactivePublisher(pub)
    }
  }

  def serverActor[T](rpc: T)(implicit system: ActorSystem, asRawRPC: AsRawRPC[T]): ActorRef = {
    val raw = asRawRPC.asRaw(rpc)
    system.actorOf(Props(new ServerActor(raw)), "rpcServerActor")
  }

  def clientActor[T](url: String)(implicit system: ActorSystem, asRealRPC: AsRealRPC[T]): T = {
    val clientRpc = new ClientRPC(url)
    asRealRPC.asReal(clientRpc)
  }

}
