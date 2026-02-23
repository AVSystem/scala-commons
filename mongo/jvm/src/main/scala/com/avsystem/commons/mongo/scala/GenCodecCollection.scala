package com.avsystem.commons
package mongo.scala

import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.avsystem.commons.serialization.GenCodec
import org.mongodb.scala.{MongoCollection, MongoDatabase}

@deprecated("Dependency on org.mongodb.scala will be removed", "2.27.0")
object GenCodecCollection {

  def create[T: ClassTag: GenCodec](
    db: MongoDatabase,
    name: String,
    legacyOptionEncoding: Boolean = GenCodecRegistry.LegacyOptionEncoding,
  ): MongoCollection[T] = {

    val newRegistry = GenCodecRegistry.create[T](db.codecRegistry, legacyOptionEncoding)
    db.withCodecRegistry(newRegistry).getCollection(name)
  }
}
