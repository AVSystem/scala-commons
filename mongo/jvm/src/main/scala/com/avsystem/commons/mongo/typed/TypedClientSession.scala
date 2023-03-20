package com.avsystem.commons
package mongo.typed

import cats.effect.ExitCase
import com.mongodb.reactivestreams.client.ClientSession
import com.mongodb.session.ServerSession
import com.mongodb.{ClientSessionOptions, ServerAddress, TransactionOptions}
import monix.eval.Task
import org.bson.{BsonDocument, BsonTimestamp}

import java.io.Closeable

/**
  * Better typed wrapper over [[ClientSession]].
  */
class TypedClientSession(val nativeSession: ClientSession)
  extends Closeable with TypedMongoUtils {

  def hasActiveTransaction: Boolean =
    nativeSession.hasActiveTransaction

  def transactionOptions: TransactionOptions =
    nativeSession.getTransactionOptions

  def startTransaction(
    transactionOptions: TransactionOptions = TransactionOptions.builder().build(),
  ): Unit =
    nativeSession.startTransaction(transactionOptions)

  def commitTransaction: Task[Unit] =
    empty(nativeSession.commitTransaction())

  def abortTransaction: Task[Unit] =
    empty(nativeSession.abortTransaction())

  /**
    * Executes a MongoDB transaction - whose contents are expressed as Monix [[Task]].
    * If the task succeeds, the transaction is committed. If the task fails, the transaction is aborted and the
    * error is propagated. The transaction is also aborted upon cancellation.
    */
  def inTransaction[T](
    transactionOptions: TransactionOptions = TransactionOptions.builder().build(),
  )(
    task: Task[T],
  ): Task[T] = Task.defer {
    startTransaction(transactionOptions)
    task.guaranteeCase {
      case ExitCase.Completed => commitTransaction
      case _ => abortTransaction
    }
  }

  def pinnedServerAddress: Option[ServerAddress] =
    Option(nativeSession.getPinnedServerAddress)

  def transactionContext(): Option[AnyRef] =
    Option(nativeSession.getTransactionContext)

  def setTransactionContext(address: ServerAddress, transactionContext: Any): Unit =
    nativeSession.setTransactionContext(address, transactionContext)

  def clearTransactionContext(): Unit =
    nativeSession.clearTransactionContext()

  def recoveryToken(): Option[BsonDocument] =
    Option(nativeSession.getRecoveryToken)

  def setRecoveryToken(recoverToken: BsonDocument): Unit =
    nativeSession.setRecoveryToken(recoverToken)

  def options: ClientSessionOptions =
    nativeSession.getOptions

  def casuallyConsistent: Boolean =
    nativeSession.isCausallyConsistent

  def originator: AnyRef =
    nativeSession.getOriginator

  def serverSession: ServerSession =
    nativeSession.getServerSession

  def operationTime: BsonTimestamp =
    nativeSession.getOperationTime

  def advanceOperationTime(operationTime: BsonTimestamp): Unit =
    nativeSession.advanceOperationTime(operationTime)

  def advanceClusterTime(clusterTime: BsonDocument): Unit =
    nativeSession.advanceClusterTime(clusterTime)

  def snapshotTimestamp: BsonTimestamp =
    nativeSession.getSnapshotTimestamp

  def setSnapshotTimestamp(snapshotTimestamp: BsonTimestamp): Unit =
    nativeSession.setSnapshotTimestamp(snapshotTimestamp)

  def clusterTime: BsonDocument =
    nativeSession.getClusterTime

  def close(): Unit =
    nativeSession.close()
}
