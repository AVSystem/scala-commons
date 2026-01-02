package com.avsystem.commons
package mongo

import org.bson.codecs.configuration.CodecRegistry
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonValue, BsonWriter}

/** @author
  *   MKej
  */
class MongoCodec[A, BSON <: BsonValue](
  bsonCodec: BsonCodec[A, BSON],
  registry: CodecRegistry,
)(implicit
  cta: ClassTag[A],
  ctbson: ClassTag[BSON],
) extends Codec[A] {

  val aClass = cta.runtimeClass.asInstanceOf[Class[A]]
  val bsonClass = ctbson.runtimeClass.asInstanceOf[Class[BSON]]

  def decode(reader: BsonReader, decoderContext: DecoderContext) =
    bsonCodec.fromBson(registry.get(bsonClass).decode(reader, decoderContext))

  def encode(writer: BsonWriter, value: A, encoderContext: EncoderContext) =
    registry.get(bsonClass).encode(writer, bsonCodec.toBson(value), encoderContext)

  def getEncoderClass = aClass
}
