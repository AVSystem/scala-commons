package com.avsystem.commons
package rpc.akka

import akka.actor.ActorPath
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawValue

/**
  * @author Wojciech Milewski
  */
private[akka] sealed trait RemoteMessage
//todo implement serialization for them, to completely remove Java Serialization from Akka RPC

private[akka] sealed trait InvocationMessage extends RemoteMessage {
  def getterChain: Seq[RawInvocation]
  def name: String
  def argLists: List[List[RawValue]]
}
private[akka] final case class ProcedureInvocationMessage(name: String, argLists: List[List[RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage
private[akka] final case class FunctionInvocationMessage(name: String, argLists: List[List[RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage
private[akka] final case class ObservableInvocationMessage(name: String, argLists: List[List[RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage

private[akka] final case class RawInvocation(rpcName: String, argLists: List[List[RawValue]]) extends RemoteMessage

private[akka] sealed trait InvocationResult extends RemoteMessage
private[akka] final case class InvocationSuccess(value: RawValue) extends InvocationResult
private[akka] final case class InvocationFailure(exceptionName: String, remoteMessage: String) extends InvocationResult

private[akka] object MonifuProtocol {
  sealed trait RemoteAck extends RemoteMessage
  case object Continue extends RemoteAck
  case object Cancel extends RemoteAck

  final case class MonifuServerPath(actorPath: ActorPath) extends RemoteMessage

  case object Subscribe extends RemoteMessage
  case object StreamCompleted extends RemoteMessage
}