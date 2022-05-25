package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.BsonValueInput
import com.avsystem.commons.serialization.GenCodec
import com.mongodb.client.model.CreateCollectionOptions
import com.mongodb.reactivestreams.client.MongoDatabase
import com.mongodb.{ReadConcern, ReadPreference, WriteConcern}
import monix.eval.Task
import monix.reactive.Observable
import org.bson.Document

class TypedMongoDatabase(
  val nativeDatabase: MongoDatabase,
  val clientSession: OptArg[TypedClientSession] = OptArg.Empty,
) extends TypedMongoUtils {
  private val sessionOrNull = clientSession.toOpt.map(_.nativeSession).orNull

  def withSession(session: TypedClientSession): TypedMongoDatabase =
    new TypedMongoDatabase(nativeDatabase, session)

  def name: String = nativeDatabase.getName
  def readPreference: ReadPreference = nativeDatabase.getReadPreference
  def writeConcern: WriteConcern = nativeDatabase.getWriteConcern
  def readConcern: ReadConcern = nativeDatabase.getReadConcern

  def withReadPreference(readPreference: ReadPreference): TypedMongoDatabase =
    new TypedMongoDatabase(nativeDatabase.withReadPreference(readPreference), clientSession)

  def withWriteConcern(writeConcern: WriteConcern): TypedMongoDatabase =
    new TypedMongoDatabase(nativeDatabase.withWriteConcern(writeConcern), clientSession)

  def withReadConcern(readConcern: ReadConcern): TypedMongoDatabase =
    new TypedMongoDatabase(nativeDatabase.withReadConcern(readConcern), clientSession)

  def getCollection[E <: BaseMongoEntity : MongoEntityMeta](name: String): TypedMongoCollection[E] =
    new TypedMongoCollection[E](nativeDatabase.getCollection(name), clientSession)

  //TODO: `runCommand`

  def drop: Task[Unit] =
    single(optionalizeFirstArg(nativeDatabase.drop(sessionOrNull))).void

  def listCollectionNames: Observable[String] =
    multi(optionalizeFirstArg(nativeDatabase.listCollectionNames(sessionOrNull)))

  def listCollections: Observable[Document] =
    multi(optionalizeFirstArg(nativeDatabase.listCollections(sessionOrNull)))

  def listCollections[T: GenCodec]: Observable[T] =
    listCollections.map(doc => BsonValueInput.read[T](doc.toBsonDocument))

  def createCollection(
    name: String,
    setupOptions: CreateCollectionOptions => CreateCollectionOptions = identity,
  ): Task[Unit] =
    single(optionalizeFirstArg(
      nativeDatabase.createCollection(sessionOrNull, name, setupOptions(new CreateCollectionOptions))
    )).void

  //TODO: `createView`, `watch`, `aggregate`
}
