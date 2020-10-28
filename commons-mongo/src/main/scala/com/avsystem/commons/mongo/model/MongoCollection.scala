package com.avsystem.commons
package mongo.model

import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.mongodb.reactivestreams.client.{FindPublisher, MongoCollection => ReactiveCollection}
import monix.reactive.Observable
import org.bson.BsonDocument
import org.bson.codecs.configuration.CodecRegistry

class MongoCollection[E: MongoAdtFormat](rawCollection: ReactiveCollection[BsonDocument]) {

  val format: MongoAdtFormat[E] = MongoAdtFormat[E]

  val nativeCollection: ReactiveCollection[E] = {
    import format._
    val codecRegistry: CodecRegistry = GenCodecRegistry.create[E](rawCollection.getCodecRegistry)
    val documentClass = classTag.runtimeClass.asInstanceOf[Class[E]]
    rawCollection.withCodecRegistry(codecRegistry).withDocumentClass(documentClass)
  }

  def find[T](
    filter: MongoDocumentFilter[E] = MongoDocumentFilter.Empty(),
    projection: MongoProjection[E, T] = MongoRef.SelfRef(format),
    sort: MongoSortOrder[E] = MongoSortOrder.Empty
    //TODO: all the freaking other options
  ): Observable[T] = {

    def setupPublisher[T0](publisher: FindPublisher[T0]): FindPublisher[T0] = publisher
      .filter((filter && projection.impliedFilter).toBson)
      .projection(projection.toProjectionBson)
      .sort(sort.toBson)

    projection match {
      case MongoRef.SelfRef(`format`) =>
        Observable.fromReactivePublisher(setupPublisher(nativeCollection.find())).asInstanceOf[Observable[T]]
      case proj =>
        Observable.fromReactivePublisher(setupPublisher(nativeCollection.find(classOf[BsonDocument]))).map(proj.decode)
    }
  }
}
