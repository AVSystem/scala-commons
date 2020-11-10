package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.mongodb.bulk.BulkWriteResult
import com.mongodb.client.model._
import com.mongodb.client.result.{DeleteResult, UpdateResult}
import com.mongodb.reactivestreams.client.{DistinctPublisher, FindPublisher, MongoCollection}
import com.mongodb.{MongoNamespace, ReadConcern, ReadPreference, WriteConcern}
import monix.eval.Task
import monix.reactive.Observable
import org.bson.codecs.configuration.CodecRegistry
import org.bson.{BsonDocument, BsonValue}
import org.reactivestreams.Publisher

final class TypedMongoCollection[E <: BaseMongoEntity : MongoAdtFormat](
  rawCollection: MongoCollection[_]
) extends DataTypeDsl[E] {
  type ID = E#IDType

  val format: MongoAdtFormat[E] = MongoAdtFormat[E]

  val SelfRef: MongoRef[E, E] = MongoRef.RootRef(format)
  val IdRef: MongoPropertyRef[E, ID] = format.fieldRefFor(SelfRef, MongoEntity.Id)

  private val docCollection = rawCollection.withDocumentClass(classOf[BsonDocument])

  val nativeCollection: MongoCollection[E] = {
    import format._
    val codecRegistry: CodecRegistry = GenCodecRegistry.create[E](rawCollection.getCodecRegistry)
    val documentClass = classTag.runtimeClass.asInstanceOf[Class[E]]
    rawCollection.withCodecRegistry(codecRegistry).withDocumentClass(documentClass)
  }

  private def single[T](publisher: Publisher[T]): Task[T] =
    Observable.fromReactivePublisher(publisher, 1).firstL

  // handles both an empty Publisher and and a single null item
  private def singleOpt[T](publisher: Publisher[T]): Task[Option[T]] =
    Observable.fromReactivePublisher(publisher, 1).filter(_ != null).firstOptionL

  def namespace: MongoNamespace =
    nativeCollection.getNamespace

  def writeConcern: WriteConcern =
    nativeCollection.getWriteConcern

  def withWriteConcern(writeConcern: WriteConcern): TypedMongoCollection[E] =
    new TypedMongoCollection(rawCollection.withWriteConcern(writeConcern))

  def readConcern: ReadConcern =
    nativeCollection.getReadConcern

  def withReadConcern(readConcern: ReadConcern): TypedMongoCollection[E] =
    new TypedMongoCollection(rawCollection.withReadConcern(readConcern))

  def readPreference: ReadPreference =
    nativeCollection.getReadPreference

  def withReadPreference(readPreference: ReadPreference): TypedMongoCollection[E] =
    new TypedMongoCollection(rawCollection.withReadPreference(readPreference))

  def drop(): Task[Unit] =
    single(nativeCollection.drop()).map(_ => ())

  def renameCollection(
    namespace: MongoNamespace,
    setupOptions: RenameCollectionOptions => RenameCollectionOptions = identity
  ): Task[Unit] =
    single(nativeCollection.renameCollection(namespace, setupOptions(new RenameCollectionOptions))).map(_ => ())

  def countDocuments(
    filter: MongoDocumentFilter[E] = MongoFilter.empty,
    setupOptions: CountOptions => CountOptions = identity
  ): Task[Long] =
    single(nativeCollection.countDocuments(filter.toBson, setupOptions(new CountOptions))).asInstanceOf[Task[Long]]

  def estimatedDocumentCount(
    setupOptions: EstimatedDocumentCountOptions => EstimatedDocumentCountOptions = identity
  ): Task[Long] =
    single(nativeCollection.estimatedDocumentCount(setupOptions(new EstimatedDocumentCountOptions))).asInstanceOf[Task[Long]]

  def exists(
    filter: MongoDocumentFilter[E],
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
  ): Task[Boolean] =
    findOne(filter, projection = MongoProjection.empty, setupOptions = setupOptions).map(_.isDefined)

  def findById(
    id: ID,
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
  ): Task[Option[E]] =
    findOne(IdRef === id, setupOptions = setupOptions)

  def findOne[T](
    filter: MongoDocumentFilter[E] = MongoFilter.empty,
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
  ): Task[Option[T]] =
    find(filter, projection, sort, o => setupOptions(o).limit(1)).firstOptionL

  def find[T](
    filter: MongoDocumentFilter[E] = MongoFilter.empty,
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
  ): Observable[T] = {

    def setupPublisher[T0](publisher: FindPublisher[T0]): FindPublisher[T0] = {
      // relying on the fact that this fluent API always returns the same object (FindPublisherImpl)
      setupOptions(publisher.asInstanceOf[FindPublisher[Any]]).asInstanceOf[FindPublisher[T0]]
        .filter(filter.toFilterBson(Opt.Empty, projection.projectionRefs))
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
        toObservable(docCollection.find()).map(proj.decodeFrom)
    }
  }

  def findOneAndUpdate[T](
    filter: MongoDocumentFilter[E],
    update: MongoDocumentUpdate[E],
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindOneAndUpdateOptions => FindOneAndUpdateOptions = identity
  ): Task[Option[T]] = {
    val filterBson = filter.toFilterBson(Opt.Empty, projection.projectionRefs)
    val options = setupOptions(new FindOneAndUpdateOptions).sort(sort.toBson)
    val (updateBson, arrayFilters) = update.toBsonAndArrayFilters
    if (!arrayFilters.isEmpty) {
      options.arrayFilters(arrayFilters)
    }
    projection match {
      case SelfRef =>
        singleOpt(nativeCollection.findOneAndUpdate(filterBson, updateBson, options).asInstanceOf[Publisher[T]])
      case proj =>
        val optionsWithProj = options.projection(proj.toProjectionBson)
        singleOpt(docCollection.findOneAndUpdate(filterBson, updateBson, optionsWithProj)).map(_.map(proj.decodeFrom))
    }
  }

  def findOneAndReplace[T](
    filter: MongoDocumentFilter[E],
    replacement: E,
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindOneAndReplaceOptions => FindOneAndReplaceOptions = identity
  ): Task[Option[T]] = {
    val filterBson = filter.toFilterBson(Opt.Empty, projection.projectionRefs)
    val options = setupOptions(new FindOneAndReplaceOptions).sort(sort.toBson)
    projection match {
      case SelfRef =>
        singleOpt(nativeCollection.findOneAndReplace(filterBson, replacement, options).asInstanceOf[Publisher[T]])
      case proj =>
        val replaceDoc = format.writeBson(replacement).asDocument
        val optionsWithProj = options.projection(proj.toProjectionBson)
        singleOpt(docCollection.findOneAndReplace(filterBson, replaceDoc, optionsWithProj)).map(_.map(proj.decodeFrom))
    }
  }

  def findOneAndDelete[T](
    filter: MongoDocumentFilter[E],
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.empty,
    setupOptions: FindOneAndDeleteOptions => FindOneAndDeleteOptions = identity
  ): Task[Option[T]] = {
    val filterBson = filter.toFilterBson(Opt.Empty, projection.projectionRefs)
    val options = setupOptions(new FindOneAndDeleteOptions).sort(sort.toBson)
    projection match {
      case SelfRef =>
        singleOpt(nativeCollection.findOneAndDelete(filterBson, options).asInstanceOf[Publisher[T]])
      case proj =>
        val optionsWithProj = options.projection(proj.toProjectionBson)
        singleOpt(docCollection.findOneAndDelete(filterBson, optionsWithProj)).map(_.map(proj.decodeFrom))
    }
  }

  def distinct[T](
    property: MongoPropertyRef[E, T],
    filter: MongoDocumentFilter[E] = MongoFilter.empty,
    setupOptions: DistinctPublisher[Any] => DistinctPublisher[Any] = identity
  ): Observable[T] = {

    val publisher = nativeCollection
      .distinct(property.filterPath, classOf[BsonValue])
      .filter(filter.toBson)

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
    update: MongoDocumentUpdate[E],
    setupOptions: UpdateOptions => UpdateOptions = identity
  ): Task[UpdateResult] = {
    val options = setupOptions(new UpdateOptions)
    val (updateBson, arrayFilters) = update.toBsonAndArrayFilters
    if (!arrayFilters.isEmpty) {
      options.arrayFilters(arrayFilters)
    }
    single(nativeCollection.updateOne(filter.toBson, updateBson, options))
  }

  def updateMany(
    filter: MongoDocumentFilter[E],
    update: MongoDocumentUpdate[E],
    setupOptions: UpdateOptions => UpdateOptions = identity
  ): Task[UpdateResult] = {
    val options = setupOptions(new UpdateOptions)
    val (updateBson, arrayFilters) = update.toBsonAndArrayFilters
    if (!arrayFilters.isEmpty) {
      options.arrayFilters(arrayFilters)
    }
    single(nativeCollection.updateMany(filter.toBson, updateBson, options))
  }

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

  def createIndex(
    index: MongoIndex[E],
    setupOptions: IndexOptions => IndexOptions = identity
  ): Task[String] =
    single(nativeCollection.createIndex(index.toBson, setupOptions(new IndexOptions)))

  def createIndexes(
    indexes: Seq[(MongoIndex[E], IndexOptions)],
    setupOptions: CreateIndexOptions => CreateIndexOptions = identity
  ): Task[String] = {
    val indexModels = indexes.iterator.map {
      case (index, options) => new IndexModel(index.toBson, options)
    }.to[JList]
    single(nativeCollection.createIndexes(indexModels, setupOptions(new CreateIndexOptions)))
  }
}
