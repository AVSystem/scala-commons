package com.avsystem.commons
package mongo

import com.avsystem.commons.mongo.TypedMongoBenchmark.BatchSize
import com.avsystem.commons.mongo.typed.{MongoEntity, MongoEntityCompanion, TypedMongoCollection}
import com.mongodb.connection.AsynchronousSocketChannelStreamFactoryFactory
import com.mongodb.reactivestreams.client.{MongoClient, MongoClients}
import com.mongodb.{ConnectionString, MongoClientSettings}
import monix.eval.Task
import monix.execution.Scheduler
import org.openjdk.jmh.annotations._

import java.nio.channels.AsynchronousChannelGroup
import java.util.concurrent.{ExecutorService, Executors}

case class BenchEntity(
  id: String,
  str: String,
  int: Int,
  list: List[String],
) extends MongoEntity[String]
object BenchEntity extends MongoEntityCompanion[BenchEntity]

object TypedMongoBenchmark {
  final val BatchSize = 1024
}

@Warmup(iterations = 2)
@Measurement(iterations = 5)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class TypedMongoBenchmark {
  val threadPool: ExecutorService = Executors.newWorkStealingPool(2)
  implicit val scheduler: Scheduler = Scheduler(threadPool)

  val mongoClientSettings: MongoClientSettings =
    MongoClientSettings.builder
      .applyConnectionString(new ConnectionString("mongodb://xpseth/test"))
      .streamFactoryFactory(AsynchronousSocketChannelStreamFactoryFactory.builder
        .group(AsynchronousChannelGroup.withThreadPool(threadPool)).build)
      .build

  val mongoClient: MongoClient = MongoClients.create(mongoClientSettings)
  val coll = new TypedMongoCollection[BenchEntity](mongoClient.getDatabase("bench").getCollection("BenchEntity"))

  val task: Task[List[Option[BenchEntity]]] =
    Task.parSequence(List.tabulate(BatchSize)(i => coll.findById(s"foo$i")))

  @Benchmark
  @OperationsPerInvocation(BatchSize)
  def benc: List[Option[BenchEntity]] =
    task.runSyncUnsafe()
}

object TypedMongoBenchmarkManual {
  def main(args: Array[String]): Unit = {
    val tmb = new TypedMongoBenchmark
    while (true) {
      tmb.benc
    }
  }
}
