package com.avsystem.commons
package mongo.typed

import com.mongodb.reactivestreams.client.MongoClients
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class TypedMongoCollectionTest extends AnyFunSuite with ScalaFutures with BeforeAndAfterEach {
  implicit val scheduler: Scheduler = Scheduler.fixedPool("test", 2)

  implicit class taskOps[T](task: Task[T]) {
    def value: T = task.runAsync.futureValue
  }

  final val Rte = RecordTestEntity

  private val client = MongoClients.create()
  private val db = client.getDatabase("test")
  private val rteColl = new TypedMongoCollection[RecordTestEntity](db.getCollection("rte"))

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    Await.result(rteColl.drop().runAsync, Duration.Inf)
  }

  test("insert one and find by ID") {
    assert(rteColl.findById(Rte.Example.id).value.isEmpty)
    rteColl.insertOne(Rte.Example).value
    assert(rteColl.findById(Rte.Example.id).value.contains(Rte.Example))
  }
}
