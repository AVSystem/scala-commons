package com.avsystem.commons
package mongo.model

import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.mongodb.client.model.{CountOptions, DeleteOptions, EstimatedDocumentCountOptions, InsertManyOptions, InsertOneOptions}
import com.mongodb.client.result.{DeleteResult, InsertManyResult, InsertOneResult}
import com.mongodb.reactivestreams.client.{FindPublisher, MongoCollection => ReactiveCollection}
import monix.eval.Task
import monix.reactive.Observable
import org.bson.BsonDocument
import org.bson.codecs.configuration.CodecRegistry
import org.reactivestreams.Publisher

class MongoCollection[E: MongoAdtFormat](rawCollection: ReactiveCollection[BsonDocument]) {

  val format: MongoAdtFormat[E] = MongoAdtFormat[E]

  val nativeCollection: ReactiveCollection[E] = {
    import format._
    val codecRegistry: CodecRegistry = GenCodecRegistry.create[E](rawCollection.getCodecRegistry)
    val documentClass = classTag.runtimeClass.asInstanceOf[Class[E]]
    rawCollection.withCodecRegistry(codecRegistry).withDocumentClass(documentClass)
  }

  private def first[T](publisher: Publisher[T]): Task[T] =
    Observable.fromReactivePublisher(publisher).firstL

  def insertOne(
    value: E,
    setupOptions: InsertOneOptions => InsertOneOptions = identity
  ): Task[InsertOneResult] =
    first(nativeCollection.insertOne(value, setupOptions(new InsertOneOptions)))

  def insertMany(
    values: Seq[E],
    setupOptions: InsertManyOptions => InsertManyOptions = identity
  ): Task[InsertManyResult] =
    first(nativeCollection.insertMany(values.asJava, setupOptions(new InsertManyOptions)))

  def deleteOne(
    filter: MongoDocumentFilter[E],
    setupOptions: DeleteOptions => DeleteOptions = identity
    //TODO: hint
  ): Task[DeleteResult] =
    first(nativeCollection.deleteOne(filter.toBson, setupOptions(new DeleteOptions)))

  def deleteMany(
    filter: MongoDocumentFilter[E],
    setupOptions: DeleteOptions => DeleteOptions = identity
    //TODO: hint
  ): Task[DeleteResult] =
    first(nativeCollection.deleteMany(filter.toBson, setupOptions(new DeleteOptions)))

  def countDocuments(
    filter: MongoDocumentFilter[E] = MongoDocumentFilter.empty,
    setupOptions: CountOptions => CountOptions = identity
    //TODO: hint
  ): Task[Long] =
    first(nativeCollection.countDocuments(filter.toBson, setupOptions(new CountOptions))).asInstanceOf[Task[Long]]

  def estimatedDocumentCount(
    setupOptions: EstimatedDocumentCountOptions => EstimatedDocumentCountOptions = identity
  ): Task[Long] =
    first(nativeCollection.estimatedDocumentCount(setupOptions(new EstimatedDocumentCountOptions))).asInstanceOf[Task[Long]]

  def find[T](
    filter: MongoDocumentFilter[E] = MongoDocumentFilter.empty,
    projection: MongoProjection[E, T] = MongoRef.SelfRef(format),
    sort: MongoSortOrder[E] = MongoSortOrder.empty,
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
    //TODO: min, max, hint
  ): Observable[T] = {

    def setupPublisher[T0](publisher: FindPublisher[T0]): FindPublisher[T0] = {
      // relying on the fact that this fluent API always returns the same object (FindPublisherImpl)
      setupOptions(publisher.asInstanceOf[FindPublisher[Any]]).asInstanceOf[FindPublisher[T0]]
        .filter((filter && projection.impliedFilter).toBson)
        .projection(projection.toProjectionBson)
        .showRecordId(projection.showRecordId)
        .sort(sort.toBson)
    }

    def toObservable[X](publisher: FindPublisher[X]): Observable[X] =
      Observable.fromReactivePublisher(setupPublisher(publisher))

    projection match {
      case MongoRef.SelfRef(`format`) =>
        toObservable(nativeCollection.find()).asInstanceOf[Observable[T]]
      case proj =>
        toObservable(nativeCollection.find(classOf[BsonDocument])).map(proj.decode)
    }
  }
}
