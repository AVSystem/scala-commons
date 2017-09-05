package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonWriter}

class GenCodecBasedBsonCodec[T](
  implicit ct: ClassTag[T], genCodec: GenCodec[T]
) extends Codec[T] {
  override def getEncoderClass = ct.runtimeClass.asInstanceOf[Class[T]]

  override def decode(reader: BsonReader, decoderContext: DecoderContext) = {
    genCodec.read(new BsonReaderInput(reader))
  }

  override def encode(writer: BsonWriter, value: T, encoderContext: EncoderContext) = {
    genCodec.write(new BsonWriterOutput(writer), value)
  }
}
