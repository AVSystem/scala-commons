package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonWriter}

class GenCodecBasedBsonCodec[T](
  legacyOptionEncoding: Boolean
)(implicit
  ct: ClassTag[T],
  genCodec: GenCodec[T],
) extends Codec[T] {
  override def getEncoderClass: Class[T] = ct.runtimeClass.asInstanceOf[Class[T]]

  override def decode(reader: BsonReader, decoderContext: DecoderContext): T =
    genCodec.read(new BsonReaderInput(reader, legacyOptionEncoding))

  override def encode(writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit =
    genCodec.write(new BsonWriterOutput(writer, legacyOptionEncoding), value)
}
