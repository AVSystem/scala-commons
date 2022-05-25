package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.BsonValueInput
import com.avsystem.commons.serialization.GenCodec
import com.mongodb._
import com.mongodb.connection.ClusterDescription
import com.mongodb.reactivestreams.client.{MongoClient, MongoClients}
import monix.eval.Task
import monix.reactive.Observable
import org.bson.Document

import java.io.Closeable

object TypedMongoClient {
  def apply(): TypedMongoClient =
    new TypedMongoClient(MongoClients.create())

  def apply(connectionString: String): TypedMongoClient =
    new TypedMongoClient(MongoClients.create(connectionString))

  def apply(connectionString: ConnectionString): TypedMongoClient =
    new TypedMongoClient(MongoClients.create(connectionString))

  def apply(settings: MongoClientSettings): TypedMongoClient =
    new TypedMongoClient(MongoClients.create(settings))

  def apply(
    connectionString: ConnectionString,
    driverInformation: MongoDriverInformation,
  ): TypedMongoClient =
    new TypedMongoClient(MongoClients.create(connectionString, driverInformation))

  def apply(
    settings: MongoClientSettings,
    driverInformation: MongoDriverInformation,
  ): TypedMongoClient =
    new TypedMongoClient(MongoClients.create(settings, driverInformation))
}

/**
  * A better-typed wrapper over [[MongoClient]]. Uses Monix [[Task]] and [[Observable]] instead of
  * [[org.reactivestreams.Publisher]]. Returns similar better-typed wrappers for database and client session objects.
  */
class TypedMongoClient(
  val nativeClient: MongoClient,
  val clientSession: OptArg[TypedClientSession] = OptArg.Empty
) extends TypedMongoUtils with Closeable {
  private val sessionOrNull = clientSession.toOpt.map(_.nativeSession).orNull

  def withSession(session: TypedClientSession): TypedMongoClient =
    new TypedMongoClient(nativeClient, session)

  def getDatabase(name: String): TypedMongoDatabase =
    new TypedMongoDatabase(nativeClient.getDatabase(name))

  def listDatabaseNames: Observable[String] =
    multi(optionalizeFirstArg(nativeClient.listDatabaseNames(sessionOrNull)))

  def listDatabases: Observable[Document] =
    multi(optionalizeFirstArg(nativeClient.listDatabases(sessionOrNull)))

  def listDatabases[T: GenCodec]: Observable[T] =
    listDatabases.map(doc => BsonValueInput.read[T](doc.toBsonDocument))

  //TODO: `watch` methods

  def startSession(
    options: ClientSessionOptions = ClientSessionOptions.builder().build()
  ): Task[TypedClientSession] =
    single(nativeClient.startSession(options)).map(new TypedClientSession(_))

  def inSession[T](
    options: ClientSessionOptions = ClientSessionOptions.builder().build()
  )(
    task: TypedClientSession => Task[T]
  ): Task[T] =
    startSession(options).bracket(task)(s => Task(s.close()))

  def inTransaction[T](
    sessionOptions: ClientSessionOptions = ClientSessionOptions.builder().build(),
    transactionOptions: TransactionOptions = TransactionOptions.builder().build(),
  )(
    task: TypedClientSession => Task[T]
  ): Task[T] =
    inSession(sessionOptions)(s => s.inTransaction(transactionOptions)(task(s)))

  def clusterDescription(): ClusterDescription =
    nativeClient.getClusterDescription

  def close(): Unit = nativeClient.close()
}
