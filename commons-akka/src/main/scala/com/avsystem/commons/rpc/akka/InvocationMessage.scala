package com.avsystem.commons
package rpc.akka

import akka.util.ByteString
import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawValue
import com.avsystem.commons.serialization.GenCodec

/**
  * @author Wojciech Milewski
  */
private sealed trait RemoteMessage extends Serializable

private object RemoteMessage {
  implicit val byteStringCodec = GenCodec.create[ByteString](input => ByteString(input.readBinary().get), (output, byteString) => output.writeBinary(byteString.toArray))
  implicit val rawInvocationCodec = GenCodec.materialize[RawInvocation]

  implicit val procedureInvocationMessageCodec = GenCodec.materialize[ProcedureInvocationMessage]
  implicit val functionInvocationMessageCodec = GenCodec.materialize[FunctionInvocationMessage]
  implicit val observableInvocationMessageCodec = GenCodec.materialize[ObservableInvocationMessage]

  implicit val invocationSuccessCodec = GenCodec.materialize[InvocationSuccess]
  implicit val invocationFailureCodec = GenCodec.materialize[InvocationFailure]

  implicit val continueCodec = GenCodec.materialize[MonifuProtocol.Continue.type]
  implicit val cancelCodec = GenCodec.materialize[MonifuProtocol.Cancel.type]
  implicit val subscribeCodec = GenCodec.materialize[MonifuProtocol.Subscribe.type]
  implicit val streamCompletedCodec = GenCodec.materialize[MonifuProtocol.StreamCompleted.type]
}

private final case class RawInvocation(rpcName: String, argLists: List[List[RawValue]]) extends RemoteMessage

private sealed trait InvocationMessage extends RemoteMessage {
  def getterChain: Seq[RawInvocation]
  def name: String
  def argLists: List[List[RawValue]]
}
private final case class ProcedureInvocationMessage(name: String, argLists: List[List[RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage
private final case class FunctionInvocationMessage(name: String, argLists: List[List[RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage
private final case class ObservableInvocationMessage(name: String, argLists: List[List[RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage

private sealed trait InvocationResult extends RemoteMessage
private final case class InvocationSuccess(value: RawValue) extends InvocationResult
private final case class InvocationFailure(exceptionName: String, remoteMessage: String) extends InvocationResult

private object MonifuProtocol {
  sealed trait RemoteAck extends RemoteMessage
  case object Continue extends RemoteAck
  case object Cancel extends RemoteAck

  case object Subscribe extends RemoteMessage
  case object StreamCompleted extends RemoteMessage
}