package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.PolymorphicGenCodec.Variant
import com.avsystem.commons.serialization.{GenCodec, ObjectInput, PolymorphicGenCodec}

class MongoWithIdPolymorphicGenCodec[T, ID](variants: Variant[_ <: T]*)(implicit idCodec: GenCodec[ID]) extends PolymorphicGenCodec[T](variants: _*){
  override def decorateInputObject(objectInput: ObjectInput): ObjectInput =
    new ObjectWithIdBsonInput[ID](objectInput.asInstanceOf[BsonReaderObjectInput])
}
