package com.avsystem.commons
package mongo.sync

import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.avsystem.commons.serialization.GenCodec
import com.mongodb.client.{MongoCollection, MongoDatabase}

object GenCodecCollection {
  def create[T: GenCodec : ClassTag](db: MongoDatabase, name: String,
    legacyOptionEncoding: Boolean = GenCodecRegistry.LegacyOptionEncoding): MongoCollection[T] = {
    val newRegistry = GenCodecRegistry.create[T](db.getCodecRegistry, legacyOptionEncoding)
    db.withCodecRegistry(newRegistry).getCollection(name, classTag[T].runtimeClass.asInstanceOf[Class[T]])
  }
}
