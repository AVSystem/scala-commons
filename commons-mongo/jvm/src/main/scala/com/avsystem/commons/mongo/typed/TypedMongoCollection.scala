package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.bincompat
import com.avsystem.commons.mongo.core.GenCodecRegistry
import com.mongodb.bulk.BulkWriteResult
import com.mongodb.client.model._
import com.mongodb.client.result._
import com.mongodb.reactivestreams.client.{DistinctPublisher, FindPublisher, MongoCollection}
import com.mongodb.{MongoNamespace, ReadConcern, ReadPreference, WriteConcern}
import monix.eval.Task
import monix.reactive.Observable
import org.bson.codecs.configuration.CodecRegistry
import org.bson.{BsonDocument, BsonValue}
import org.reactivestreams.Publisher

class TypedMongoCollection[E <: BaseMongoEntity] private(
  /** The native (Reactive Streams driver) collection */
  val nativeCollection: MongoCollection[E],
  docCollection: MongoCollection[BsonDocument],
  val clientSession: Opt[TypedClientSession],
)(
  implicit meta: MongoEntityMeta[E]
) extends DataTypeDsl[E] with TypedMongoUtils {

  def this(
    rawCollection: MongoCollection[_],
    clientSession: OptArg[TypedClientSession] = OptArg.Empty
  )(implicit
    meta: MongoEntityMeta[E]
  ) = this(
    TypedMongoCollection.mkNativeCollection[E](rawCollection),
    rawCollection.withDocumentClass(classOf[BsonDocument]),
    clientSession.toOpt
  )

  type ID = E#IDType

  val format: MongoAdtFormat[E] = meta.format

  val SelfRef: MongoRef[E, E] = MongoRef.RootRef(format)
  val IdRef: MongoPropertyRef[E, ID] = meta.idRef

  private val sessionOrNull = clientSession.map(_.nativeSession).orNull

  def withSession(session: TypedClientSession): TypedMongoCollection[E] =
    new TypedMongoCollection(nativeCollection, docCollection, session.opt)

  def namespace: MongoNamespace = nativeCollection.getNamespace
  def writeConcern: WriteConcern = nativeCollection.getWriteConcern
  def readConcern: ReadConcern = nativeCollection.getReadConcern
  def readPreference: ReadPreference = nativeCollection.getReadPreference

  def withWriteConcern(writeConcern: WriteConcern): TypedMongoCollection[E] =
    new TypedMongoCollection(
      nativeCollection.withWriteConcern(writeConcern),
      docCollection.withWriteConcern(writeConcern),
      clientSession,
    )

  def withReadConcern(readConcern: ReadConcern): TypedMongoCollection[E] =
    new TypedMongoCollection(
      nativeCollection.withReadConcern(readConcern),
      docCollection.withReadConcern(readConcern),
      clientSession,
    )

  def withReadPreference(readPreference: ReadPreference): TypedMongoCollection[E] =
    new TypedMongoCollection(
      nativeCollection.withReadPreference(readPreference),
      docCollection.withReadPreference(readPreference),
      clientSession,
    )

  /**
    * Invokes some empty-result operation directly on Reactive Streams collection. This method is supposed
    * to be used for database operations not covered directly by [[TypedMongoCollection]].
    */
  def emptyResultNativeOp(operation: MongoCollection[E] => Publisher[Void]): Task[Unit] =
    empty(operation(nativeCollection))

  /**
    * Invokes some single-result operation directly on Reactive Streams collection. This method is supposed
    * to be used for database operations not covered directly by [[TypedMongoCollection]].
    */
  def singleResultNativeOp[T](operation: MongoCollection[E] => Publisher[T]): Task[T] =
    single(operation(nativeCollection))

  /**
    * Invokes some multiple-result operation directly on Reactive Streams collection. This method is supposed
    * to be used for database operations not covered directly by [[TypedMongoCollection]].
    */
  def multiResultNativeOp[T](operation: MongoCollection[E] => Publisher[T]): Observable[T] =
    Observable.fromReactivePublisher(operation(nativeCollection))

  def drop(): Task[Unit] =
    empty(optionalizeFirstArg(nativeCollection.drop(sessionOrNull)))

  def renameCollection(
    namespace: MongoNamespace,
    setupOptions: RenameCollectionOptions => RenameCollectionOptions = identity
  ): Task[Unit] =
    empty(optionalizeFirstArg(
      nativeCollection.renameCollection(sessionOrNull, namespace, setupOptions(new RenameCollectionOptions))))

  def countDocuments(
    filter: MongoDocumentFilter[E] = MongoFilter.empty,
    setupOptions: CountOptions => CountOptions = identity
  ): Task[Long] =
    single(optionalizeFirstArg(
      nativeCollection.countDocuments(sessionOrNull, filter.toBson, setupOptions(new CountOptions))
    )).asInstanceOf[Task[Long]]

  def estimatedDocumentCount(
    setupOptions: EstimatedDocumentCountOptions => EstimatedDocumentCountOptions = identity
  ): Task[Long] =
    single(optionalizeFirstArg(
      nativeCollection.estimatedDocumentCount(setupOptions(new EstimatedDocumentCountOptions))
    )).asInstanceOf[Task[Long]]

  def exists(
    filter: MongoDocumentFilter[E],
    setupOptions: CountOptions => CountOptions = identity
  ): Task[Boolean] =
    countDocuments(filter, options => setupOptions(options).limit(1)).map(_ > 0)

  def findById(
    id: ID,
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
  ): Task[Option[E]] =
    findOne(IdRef === id, setupOptions = setupOptions)

  def findOne[T](
    filter: MongoDocumentFilter[E] = MongoFilter.empty,
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.unspecified,
    setupOptions: FindPublisher[Any] => FindPublisher[Any] = identity
  ): Task[Option[T]] =
    find(filter, projection, sort, o => setupOptions(o).limit(1)).firstOptionL

  def find[T](
    filter: MongoDocumentFilter[E] = MongoFilter.empty,
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.unspecified,
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
        toObservable(optionalizeFirstArg(nativeCollection.find(sessionOrNull))).asInstanceOf[Observable[T]]
      case proj =>
        toObservable(optionalizeFirstArg(docCollection.find(sessionOrNull))).map(proj.decodeFrom)
    }
  }

  def findOneAndUpdate[T](
    filter: MongoDocumentFilter[E],
    update: MongoDocumentUpdate[E],
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.unspecified,
    upsert: Boolean = false, // extracted as separate param because it's very commonly used
    setupOptions: FindOneAndUpdateOptions => FindOneAndUpdateOptions = identity
  ): Task[Option[T]] = {
    val filterBson = filter.toFilterBson(Opt.Empty, projection.projectionRefs)
    val options = setupOptions(new FindOneAndUpdateOptions).sort(sort.toBson).upsert(upsert)
    val (updateBson, arrayFilters) = update.toBsonAndArrayFilters
    if (!arrayFilters.isEmpty) {
      options.arrayFilters(arrayFilters)
    }
    projection match {
      case SelfRef =>
        singleOpt(optionalizeFirstArg(
          nativeCollection.findOneAndUpdate(sessionOrNull, filterBson, updateBson, options)).asInstanceOf[Publisher[T]])
      case proj =>
        val optionsWithProj = options.projection(proj.toProjectionBson)
        singleOpt(optionalizeFirstArg(
          docCollection.findOneAndUpdate(sessionOrNull, filterBson, updateBson, optionsWithProj)
        )).map(_.map(proj.decodeFrom))
    }
  }

  def findOneAndReplace[T](
    filter: MongoDocumentFilter[E],
    replacement: E,
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.unspecified,
    upsert: Boolean = false, // extracted as separate param because it's very commonly used
    setupOptions: FindOneAndReplaceOptions => FindOneAndReplaceOptions = identity
  ): Task[Option[T]] = {
    val filterBson = filter.toFilterBson(Opt.Empty, projection.projectionRefs)
    val options = setupOptions(new FindOneAndReplaceOptions).sort(sort.toBson).upsert(upsert)
    projection match {
      case SelfRef =>
        singleOpt(optionalizeFirstArg(
          nativeCollection.findOneAndReplace(sessionOrNull, filterBson, replacement, options)
        ).asInstanceOf[Publisher[T]])
      case proj =>
        val replaceDoc = format.writeBson(replacement).asDocument
        val optionsWithProj = options.projection(proj.toProjectionBson)
        singleOpt(optionalizeFirstArg(
          docCollection.findOneAndReplace(sessionOrNull, filterBson, replaceDoc, optionsWithProj)
        )).map(_.map(proj.decodeFrom))
    }
  }

  def findOneAndDelete[T](
    filter: MongoDocumentFilter[E],
    projection: MongoProjection[E, T] = SelfRef,
    sort: MongoDocumentOrder[E] = MongoDocumentOrder.unspecified,
    setupOptions: FindOneAndDeleteOptions => FindOneAndDeleteOptions = identity
  ): Task[Option[T]] = {
    val filterBson = filter.toFilterBson(Opt.Empty, projection.projectionRefs)
    val options = setupOptions(new FindOneAndDeleteOptions).sort(sort.toBson)
    projection match {
      case SelfRef =>
        singleOpt(optionalizeFirstArg(
          nativeCollection.findOneAndDelete(sessionOrNull, filterBson, options)).asInstanceOf[Publisher[T]])
      case proj =>
        val optionsWithProj = options.projection(proj.toProjectionBson)
        singleOpt(optionalizeFirstArg(
          docCollection.findOneAndDelete(sessionOrNull, filterBson, optionsWithProj))).map(_.map(proj.decodeFrom))
    }
  }

  def distinct[T](
    property: MongoPropertyRef[E, T],
    filter: MongoDocumentFilter[E] = MongoFilter.empty,
    setupOptions: DistinctPublisher[Any] => DistinctPublisher[Any] = identity
  ): Observable[T] = {

    val publisher =
      optionalizeFirstArg(nativeCollection.distinct(sessionOrNull, property.rawPath, classOf[BsonValue]))
        .filter(filter.toFilterBson(Opt.Empty, property.projectionRefs))

    val publisherWithOptions =
      setupOptions(publisher.asInstanceOf[DistinctPublisher[Any]]).asInstanceOf[DistinctPublisher[BsonValue]]

    Observable.fromReactivePublisher(publisherWithOptions).map(property.format.readBson)
  }

  def insertOne(
    value: E,
    setupOptions: InsertOneOptions => InsertOneOptions = identity
  ): Task[InsertOneResult] =
    single(optionalizeFirstArg(
      nativeCollection.insertOne(sessionOrNull, value, setupOptions(new InsertOneOptions))))

  def insertMany(
    values: Seq[E],
    setupOptions: InsertManyOptions => InsertManyOptions = identity
  ): Task[InsertManyResult] =
    single(optionalizeFirstArg(
      nativeCollection.insertMany(sessionOrNull, values.asJava, setupOptions(new InsertManyOptions))))

  def deleteOne(
    filter: MongoDocumentFilter[E],
    setupOptions: DeleteOptions => DeleteOptions = identity
  ): Task[DeleteResult] =
    single(optionalizeFirstArg(
      nativeCollection.deleteOne(sessionOrNull, filter.toBson, setupOptions(new DeleteOptions))))

  def deleteMany(
    filter: MongoDocumentFilter[E],
    setupOptions: DeleteOptions => DeleteOptions = identity
  ): Task[DeleteResult] =
    single(optionalizeFirstArg(
      nativeCollection.deleteMany(sessionOrNull, filter.toBson, setupOptions(new DeleteOptions))))

  def updateOne(
    filter: MongoDocumentFilter[E],
    update: MongoDocumentUpdate[E],
    upsert: Boolean = false, // extracted as separate param because it's very commonly used
    setupOptions: UpdateOptions => UpdateOptions = identity
  ): Task[UpdateResult] = {
    val options = setupOptions(new UpdateOptions).upsert(upsert)
    val (updateBson, arrayFilters) = update.toBsonAndArrayFilters
    if (!arrayFilters.isEmpty) {
      options.arrayFilters(arrayFilters)
    }
    single(optionalizeFirstArg(
      nativeCollection.updateOne(sessionOrNull, filter.toBson, updateBson, options)))
  }

  def updateMany(
    filter: MongoDocumentFilter[E],
    update: MongoDocumentUpdate[E],
    upsert: Boolean = false, // extracted as separate param because it's very commonly used
    setupOptions: UpdateOptions => UpdateOptions = identity
  ): Task[UpdateResult] = {
    val options = setupOptions(new UpdateOptions).upsert(upsert)
    val (updateBson, arrayFilters) = update.toBsonAndArrayFilters
    if (!arrayFilters.isEmpty) {
      options.arrayFilters(arrayFilters)
    }
    single(optionalizeFirstArg(
      nativeCollection.updateMany(sessionOrNull, filter.toBson, updateBson, options)))
  }

  def replaceOne(
    filter: MongoDocumentFilter[E],
    replacement: E,
    upsert: Boolean = false, // extracted as separate param because it's very commonly used
    setupOptions: ReplaceOptions => ReplaceOptions = identity
  ): Task[UpdateResult] = {
    val options = setupOptions(new ReplaceOptions).upsert(upsert)
    single(optionalizeFirstArg(
      nativeCollection.replaceOne(sessionOrNull, filter.toBson, replacement, options)))
  }

  def bulkWrite(
    writes: Seq[MongoWrite[E]],
    setupOptions: BulkWriteOptions => BulkWriteOptions = identity
  ): Task[BulkWriteResult] = {
    val requests = writes.iterator.map(_.toWriteModel).to(JList)
    single(optionalizeFirstArg(
      nativeCollection.bulkWrite(sessionOrNull, requests, setupOptions(new BulkWriteOptions))))
  }

  def createIndex(index: MongoIndex[E]): Task[String] =
    single(optionalizeFirstArg(
      nativeCollection.createIndex(sessionOrNull, index.toBson, index.setupOptions(new IndexOptions))))

  def createIndexes(
    indexes: Seq[MongoIndex[E]],
    setupOptions: CreateIndexOptions => CreateIndexOptions = identity
  ): Task[String] = {
    val indexModels = indexes.iterator
      .map(index => new IndexModel(index.toBson, index.setupOptions(new IndexOptions)))
      .to(JList)
    single(optionalizeFirstArg(
      nativeCollection.createIndexes(sessionOrNull, indexModels, setupOptions(new CreateIndexOptions))))
  }

  @bincompat private[typed] def this(rawCollection: MongoCollection[_], format: MongoAdtFormat[E]) =
    this(rawCollection)(MongoEntityMeta.bincompatMeta(format))
}

object TypedMongoCollection {
  private def mkNativeCollection[E <: BaseMongoEntity : MongoEntityMeta](
    rawCollection: MongoCollection[_]
  )(implicit
    meta: MongoEntityMeta[E]
  ): MongoCollection[E] = {
    import meta.format._
    val codecRegistry: CodecRegistry = GenCodecRegistry.create[E](rawCollection.getCodecRegistry)
    val documentClass = classTag.runtimeClass.asInstanceOf[Class[E]]
    rawCollection.withCodecRegistry(codecRegistry).withDocumentClass(documentClass)
  }
}
