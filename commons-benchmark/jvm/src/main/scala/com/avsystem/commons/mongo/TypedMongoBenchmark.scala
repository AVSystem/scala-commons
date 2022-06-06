package com.avsystem.commons
package mongo

import com.avsystem.commons.concurrent.AutoPipeliner
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
  final val BatchSize = 1024 * 128
}

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class TypedMongoBenchmark {
  val threadPool: ExecutorService = Executors.newWorkStealingPool(6)
  implicit val scheduler: Scheduler = Scheduler(threadPool)

  private val pipeliner = new AutoPipeliner[String, BenchEntity](
    ids => {
      coll
        .find(coll.IdRef.in(ids))
        .foldLeftL(new MHashMap[String, BenchEntity]) { (acc, entity) =>
          acc(entity.id) = entity
          acc
        }
        .map(m => ids.map(m.apply).toList)
    },
    maxBatchSize = 7144,
    parallelism = 48,
  )

  private val mongoClientSettings: MongoClientSettings =
    MongoClientSettings.builder
      .applyConnectionString(new ConnectionString("mongodb://xpseth/bench"))
      .streamFactoryFactory(AsynchronousSocketChannelStreamFactoryFactory.builder
        .group(AsynchronousChannelGroup.withThreadPool(threadPool)).build)
      .build

  private val mongoClient: MongoClient = MongoClients.create(mongoClientSettings)
  val coll: TypedMongoCollection[BenchEntity] = new TypedMongoCollection[BenchEntity](
    mongoClient.getDatabase("bench").getCollection("BenchEntity"))

  val task: Task[List[BenchEntity]] =
    Task.parSequence(List.tabulate(BatchSize)(i => pipeliner.exec(s"foo${i * 7817 % 100000}")))

  @Benchmark
  @OperationsPerInvocation(BatchSize)
  def benc: List[BenchEntity] =
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
