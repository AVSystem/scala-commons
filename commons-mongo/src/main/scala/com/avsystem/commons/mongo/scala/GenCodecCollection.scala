package com.avsystem.commons
package mongo.scala

import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.avsystem.commons.serialization.GenCodec
import org.mongodb.scala.{MongoCollection, MongoDatabase}

object GenCodecCollection {
  def create[T: ClassTag : GenCodec](db: MongoDatabase, name: String): MongoCollection[T] = {
    val newRegistry = GenCodecRegistry.create[T](db.codecRegistry)
    db.withCodecRegistry(newRegistry).getCollection(name)
  }
}
