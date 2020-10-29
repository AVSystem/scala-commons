package com.avsystem.commons
package mongo.sync

import com.avsystem.commons.mongo.{BsonCodec, MongoCodec}
import com.mongodb.client.{FindIterable, MongoCollection, MongoDatabase}
import org.bson.BsonDocument
import org.bson.codecs.configuration.CodecRegistries
import org.bson.conversions.Bson

/**
  * @author MKej
  */
trait MongoOps {

  import MongoOps._

  implicit def dbOps(db: MongoDatabase): DBOps = new DBOps(db)
  implicit def findIterableOps[T](find: FindIterable[T]): FindIterableOps[T] = new FindIterableOps(find)
}

object MongoOps {
  final class DBOps(private val db: MongoDatabase) extends AnyVal {
    def getCollection[A](name: String, codec: BsonCodec[A, BsonDocument])(implicit ct: ClassTag[A]): MongoCollection[A] = {
      val mongoCodec = new MongoCodec[A, BsonDocument](codec, db.getCodecRegistry)
      val registry = CodecRegistries.fromRegistries(
        CodecRegistries.fromCodecs(mongoCodec),
        db.getCodecRegistry
      )
      db.getCollection(name, ct.runtimeClass.asInstanceOf[Class[A]]).withCodecRegistry(registry)
    }
  }

  final class FindIterableOps[T](private val find: FindIterable[T]) extends AnyVal {
    def firstOpt: Option[T] = Option(find.first)

    def page(sort: Bson, offset: Int, maxItems: Int): Vector[T] = {
      val b = Vector.newBuilder[T]
      find.sort(sort).skip(offset).limit(maxItems).forEach(new JConsumer[T] {
        def accept(t: T) = b += t
      })
      b.result()
    }
  }
}
