package com.avsystem.commons
package rpc.akka.serialization

import akka.serialization.Serializer
import akka.util.ByteString
import com.avsystem.commons.rpc.akka._
import com.avsystem.commons.serialization.GenCodec

/**
  * @author Wojciech Milewski
  */
final class RemoteMessageSerializer extends Serializer {

  import RemoteMessage._

  override def identifier: Int = 123532
  override def includeManifest: Boolean = true
  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    require(manifest.isDefined)

    val input = new ByteStringInput(ByteString(bytes))

    manifest.get match {
      case c if c == classOf[ProcedureInvocationMessage] => GenCodec.read[ProcedureInvocationMessage](input)
      case c if c == classOf[FunctionInvocationMessage] => GenCodec.read[FunctionInvocationMessage](input)
      case c if c == classOf[ObservableInvocationMessage] => GenCodec.read[ObservableInvocationMessage](input)
      case c if c == classOf[RawInvocation] => GenCodec.read[RawInvocation](input)
      case c if c == classOf[InvocationSuccess] => GenCodec.read[InvocationSuccess](input)
      case c if c == classOf[InvocationFailure] => GenCodec.read[InvocationFailure](input)
      case c if c == MonifuProtocol.Continue.getClass => GenCodec.read[MonifuProtocol.Continue.type](input)
      case c if c == MonifuProtocol.Cancel.getClass => GenCodec.read[MonifuProtocol.Cancel.type](input)
      case c if c == MonifuProtocol.Subscribe.getClass => GenCodec.read[MonifuProtocol.Subscribe.type](input)
      case c if c == MonifuProtocol.StreamCompleted.getClass => GenCodec.read[MonifuProtocol.StreamCompleted.type](input)
      case c if c == MonifuProtocol.Heartbeat.getClass => GenCodec.read[MonifuProtocol.Heartbeat.type](input)
    }
  }

  override def toBinary(o: AnyRef): Array[Byte] = {
    require(o.isInstanceOf[RemoteMessage])

    val output = new ByteStringOutput
    o.asInstanceOf[RemoteMessage] match {
      case m: ProcedureInvocationMessage => GenCodec.write(output, m)
      case m: FunctionInvocationMessage => GenCodec.write(output, m)
      case m: ObservableInvocationMessage => GenCodec.write(output, m)
      case m: RawInvocation => GenCodec.write(output, m)
      case m: InvocationSuccess => GenCodec.write(output, m)
      case m: InvocationFailure => GenCodec.write(output, m)
      case MonifuProtocol.Continue => GenCodec.write(output, MonifuProtocol.Continue)
      case MonifuProtocol.Cancel => GenCodec.write(output, MonifuProtocol.Cancel)
      case MonifuProtocol.Subscribe => GenCodec.write(output, MonifuProtocol.Subscribe)
      case MonifuProtocol.StreamCompleted => GenCodec.write(output, MonifuProtocol.StreamCompleted)
      case MonifuProtocol.Heartbeat => GenCodec.write(output, MonifuProtocol.Heartbeat)
    }

    output.result.toArray
  }

}
