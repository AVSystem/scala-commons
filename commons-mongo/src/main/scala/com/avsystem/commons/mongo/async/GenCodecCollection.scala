package com.avsystem.commons
package mongo.async

import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.avsystem.commons.serialization.GenCodec
import com.mongodb.async.client.{MongoCollection, MongoDatabase}

object GenCodecCollection {
  def create[T: GenCodec](db: MongoDatabase, name: String)(implicit ct: ClassTag[T]): MongoCollection[T] = {
    val newRegistry = GenCodecRegistry.create[T](db.getCodecRegistry)
    db.withCodecRegistry(newRegistry).getCollection(name, ct.runtimeClass.asInstanceOf[Class[T]])
  }
}
