package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.mongodb.bulk.BulkWriteResult
import com.mongodb.client.model._
import com.mongodb.client.result.{DeleteResult, UpdateResult}
import com.mongodb.reactivestreams.client.{DistinctPublisher, FindPublisher, MongoCollection}
import monix.eval.Task
import monix.reactive.Observable
import org.bson.codecs.configuration.CodecRegistry
import org.bson.{BsonDocument, BsonValue}
import org.reactivestreams.Publisher

final class TypedMongoCollection[E <: BaseMongoEntity : MongoAdtFormat](
  rawCollection: MongoCollection[_]
) {
  type ID = E#IDType

  val format: MongoAdtFormat[E] = MongoAdtFormat[E]

  val SelfRef: MongoDataRef[E, E] = MongoRef.SelfRef(format)
  val IdRef: MongoPropertyRef[E, ID] = format.fieldRefFor(SelfRef, MongoEntity.Id)

  private val docCollection = rawCollection.withDocumentClass(classOf[BsonDocument])

  val nativeCollection: MongoCollection[E] = {
    import format._
    val codecRegistry: CodecRegistry = GenCodecRegistry.create[E](rawCollection.getCodecRegistry)
    val documentClass = classTag.runtimeClass.asInstanceOf[Class[E]]
    rawCollection.withCodecRegistry(codecRegistry).withDocumentClass(documentClass)
  }

  private def single[T](publisher: Publisher[T]): Task[T] =
    Observable.fromReactivePublisher(publisher).firstL

  private def nullableSingle[T](publisher: Publisher[T]): Task[Option[T]] =
    single(publisher).map(_.option)

  def countDocuments(
    filter: MongoDocumentFilter[E] = MongoDocumentFilter.empty,
    setupOptions: CountOptions => CountOptions = identity
  ): Task[Long] =
    single(nativeCollection.countDocuments(filter.toBson, setupOptions(new CountOptions))).asInstanceOf[Task[Long]]

  def estimatedDocumentCount(
    setupOptions: EstimatedDocumentCountOptions => EstimatedDocumentCountOptions = identity
  ): Task[Long] =
    single(nativeCollection.estimatedDocumentCount(setupOptions(new EstimatedDocumentCountOptions))).asInstanceOf[Task[Long]]

  def findById(
    id: ID,
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
  ): Task[Option[E]] =
    find(IdRef === id, setupOptions = publisher => setupOptions(publisher).limit(1)).firstOptionL

  def find[T](
    filter: MongoDocumentFilter[E] = MongoDocumentFilter.empty,
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
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
      case SelfRef =>
        toObservable(nativeCollection.find()).asInstanceOf[Observable[T]]
      case proj =>
        toObservable(docCollection.find()).map(proj.decode)
    }
  }

  def findOneAndUpdate[T](
    filter: MongoDocumentFilter[E],
    update: MongoUpdate[E],
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindOneAndUpdateOptions => FindOneAndUpdateOptions = identity
  ): Task[Option[T]] = projection match {
    case SelfRef =>
      val options = setupOptions(new FindOneAndUpdateOptions).sort(sort.toBson)
      nullableSingle(nativeCollection.findOneAndUpdate(filter.toBson, update.toBson, options).asInstanceOf[Publisher[T]])
    case proj =>
      val options = setupOptions(new FindOneAndUpdateOptions).projection(proj.toProjectionBson).sort(sort.toBson)
      nullableSingle(docCollection.findOneAndUpdate(filter.toBson, update.toBson, options)).map(_.map(proj.decode))
  }

  def findOneAndReplace[T](
    filter: MongoDocumentFilter[E],
    replacement: E,
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindOneAndReplaceOptions => FindOneAndReplaceOptions = identity
  ): Task[Option[T]] = projection match {
    case SelfRef =>
      val options = setupOptions(new FindOneAndReplaceOptions).sort(sort.toBson)
      nullableSingle(nativeCollection.findOneAndReplace(filter.toBson, replacement, options).asInstanceOf[Publisher[T]])
    case proj =>
      val options = setupOptions(new FindOneAndReplaceOptions).projection(proj.toProjectionBson).sort(sort.toBson)
      nullableSingle(docCollection.findOneAndReplace(filter.toBson, format.writeBson(replacement).asDocument(), options)).map(_.map(proj.decode))
  }

  def findOneAndDelete[T](
    filter: MongoDocumentFilter[E],
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindOneAndDeleteOptions => FindOneAndDeleteOptions = identity
  ): Task[Option[T]] = projection match {
    case SelfRef =>
      val options = setupOptions(new FindOneAndDeleteOptions).sort(sort.toBson)
      nullableSingle(nativeCollection.findOneAndDelete(filter.toBson, options).asInstanceOf[Publisher[T]])
    case proj =>
      val options = setupOptions(new FindOneAndDeleteOptions).projection(proj.toProjectionBson).sort(sort.toBson)
      nullableSingle(docCollection.findOneAndDelete(filter.toBson, options)).map(_.map(proj.decode))
  }

  def distinct[T](
    property: MongoPropertyRef[E, T],
    filter: MongoDocumentFilter[E] = MongoDocumentFilter.empty,
    setupOptions: DistinctPublisher[Any] => DistinctPublisher[Any] = identity
  ): Observable[T] = {

    val publisher = nativeCollection
      .distinct(property.propertyPath, classOf[BsonValue])
      .filter((property.impliedFilter && filter).toBson)

    val publisherWithOptions =
      setupOptions(publisher.asInstanceOf[DistinctPublisher[Any]]).asInstanceOf[DistinctPublisher[BsonValue]]

    Observable.fromReactivePublisher(publisherWithOptions).map(property.format.readBson)
  }

  def insertOne(
    value: E,
    setupOptions: InsertOneOptions => InsertOneOptions = identity
  ): Task[Unit] =
    single(nativeCollection.insertOne(value, setupOptions(new InsertOneOptions))).map(_ => ())

  def insertMany(
    values: Seq[E],
    setupOptions: InsertManyOptions => InsertManyOptions = identity
  ): Task[Unit] =
    single(nativeCollection.insertMany(values.asJava, setupOptions(new InsertManyOptions))).map(_ => ())

  def deleteOne(
    filter: MongoDocumentFilter[E],
    setupOptions: DeleteOptions => DeleteOptions = identity
  ): Task[DeleteResult] =
    single(nativeCollection.deleteOne(filter.toBson, setupOptions(new DeleteOptions)))

  def deleteMany(
    filter: MongoDocumentFilter[E],
    setupOptions: DeleteOptions => DeleteOptions = identity
  ): Task[DeleteResult] =
    single(nativeCollection.deleteMany(filter.toBson, setupOptions(new DeleteOptions)))

  def updateOne(
    filter: MongoDocumentFilter[E],
    update: MongoUpdate[E],
    setupOptions: UpdateOptions => UpdateOptions = identity
  ): Task[UpdateResult] =
    single(nativeCollection.updateOne(filter.toBson, update.toBson, setupOptions(new UpdateOptions)))

  def updateMany(
    filter: MongoDocumentFilter[E],
    update: MongoUpdate[E],
    setupOptions: UpdateOptions => UpdateOptions = identity
  ): Task[UpdateResult] =
    single(nativeCollection.updateMany(filter.toBson, update.toBson, setupOptions(new UpdateOptions)))

  def replaceOne(
    filter: MongoDocumentFilter[E],
    replacement: E,
    setupOptions: ReplaceOptions => ReplaceOptions = identity
  ): Task[UpdateResult] =
    single(nativeCollection.replaceOne(filter.toBson, replacement, setupOptions(new ReplaceOptions)))

  def bulkWrite(
    writes: Seq[MongoWrite[E]],
    setupOptions: BulkWriteOptions => BulkWriteOptions = identity
  ): Task[BulkWriteResult] = {
    val requests = writes.iterator.map(_.toWriteModel).to[JList]
    single(nativeCollection.bulkWrite(requests, setupOptions(new BulkWriteOptions)))
  }
}
