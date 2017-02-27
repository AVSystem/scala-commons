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

    val input = new ByteStringLinearInput(ByteString(bytes))

    manifest.get match {
      case c if c == classOf[ProcedureInvocationMessage] => GenCodec.read[ProcedureInvocationMessage](input)
      case c if c == classOf[FunctionInvocationMessage] => GenCodec.read[FunctionInvocationMessage](input)
      case c if c == classOf[ObservableInvocationMessage] => GenCodec.read[ObservableInvocationMessage](input)
      case c if c == classOf[RawInvocation] => GenCodec.read[RawInvocation](input)
      case c if c == classOf[InvocationSuccess] => GenCodec.read[InvocationSuccess](input)
      case c if c == classOf[InvocationFailure] => GenCodec.read[InvocationFailure](input)
      case c if c == MonixProtocol.Continue.getClass => GenCodec.read[MonixProtocol.Continue.type](input)
      case c if c == MonixProtocol.Stop.getClass => GenCodec.read[MonixProtocol.Stop.type](input)
      case c if c == MonixProtocol.Subscribe.getClass => GenCodec.read[MonixProtocol.Subscribe.type](input)
      case c if c == MonixProtocol.StreamCompleted.getClass => GenCodec.read[MonixProtocol.StreamCompleted.type](input)
      case c if c == MonixProtocol.Heartbeat.getClass => GenCodec.read[MonixProtocol.Heartbeat.type](input)
    }
  }

  override def toBinary(o: AnyRef): Array[Byte] = {
    require(o.isInstanceOf[RemoteMessage])

    val output = new ByteStringLinearOutput(ByteString.newBuilder)
    o.asInstanceOf[RemoteMessage] match {
      case m: ProcedureInvocationMessage => GenCodec.write(output, m)
      case m: FunctionInvocationMessage => GenCodec.write(output, m)
      case m: ObservableInvocationMessage => GenCodec.write(output, m)
      case m: RawInvocation => GenCodec.write(output, m)
      case m: InvocationSuccess => GenCodec.write(output, m)
      case m: InvocationFailure => GenCodec.write(output, m)
      case MonixProtocol.Continue => GenCodec.write(output, MonixProtocol.Continue)
      case MonixProtocol.Stop => GenCodec.write(output, MonixProtocol.Stop)
      case MonixProtocol.Subscribe => GenCodec.write(output, MonixProtocol.Subscribe)
      case MonixProtocol.StreamCompleted => GenCodec.write(output, MonixProtocol.StreamCompleted)
      case MonixProtocol.Heartbeat => GenCodec.write(output, MonixProtocol.Heartbeat)
    }

    output.result.toArray
  }

}
