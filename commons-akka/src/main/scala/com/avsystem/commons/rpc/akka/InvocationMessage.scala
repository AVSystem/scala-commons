package com.avsystem.commons
package rpc.akka

import com.avsystem.commons.rpc.akka.AkkaRPCFramework.RawInvocation

/**
  * @author Wojciech Milewski
  */
private[akka] sealed trait InvocationMessage {
  def getterChain: Seq[RawInvocation]
  def name: String
  def argLists: List[List[AkkaRPCFramework.RawValue]]
}
private[akka] final case class ProcedureInvocationMessage(name: String, argLists: List[List[AkkaRPCFramework.RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage
private[akka] final case class FunctionInvocationMessage(name: String, argLists: List[List[AkkaRPCFramework.RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage
private[akka] final case class ObservableInvocationMessage(name: String, argLists: List[List[AkkaRPCFramework.RawValue]], getterChain: Seq[RawInvocation]) extends InvocationMessage