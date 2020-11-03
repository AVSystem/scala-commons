package com.avsystem.commons
package mongo.typed

import com.mongodb.reactivestreams.client.MongoClients
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class TypedMongoCollectionTest extends AnyFunSuite with ScalaFutures with BeforeAndAfterAll {
  implicit val scheduler: Scheduler = Scheduler.fixedPool("test", 2)

  implicit class taskOps[T](task: Task[T]) {
    def value: T = task.runAsync.futureValue
  }

  final val Rte = RecordTestEntity

  private val client = MongoClients.create()
  private val db = client.getDatabase("test")
  private val rteColl = new TypedMongoCollection[RecordTestEntity](db.getCollection("rte"))

  private val entities = (0 until 100).map { i =>
    val innerRecord = InnerRecord(
      i, "istr", Opt("istropt"), Opt.Empty, List(3, 4, 5), Map("ione" -> 1, "ithree" -> 3)
    )

    RecordTestEntity(
      s"rid$i", i, "str", Opt("stropt"), Opt(i).filter(_ % 2 == 0),
      List(1, 2, 3), Map("one" -> 1, "two" -> 2), innerRecord,
      Opt(innerRecord), List(innerRecord), Map(InnerId("iid") -> innerRecord),
      Opt(Map(InnerId("iid") -> List(innerRecord)))
    )
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(rteColl.drop().runAsync, Duration.Inf)
    Await.result(rteColl.insertMany(entities).runAsync, Duration.Inf)
  }

  test("findById") {
    assert(rteColl.findById(Rte.Example.id).value.isEmpty)
    assert(rteColl.findById(entities.head.id).value.contains(entities.head))
  }

  test("countDocuments") {
    assert(rteColl.countDocuments().value == 100)
    assert(rteColl.countDocuments(Rte.ref(_.int) < 10).value == 10)
  }

  test("find") {
    assert(rteColl.find().toListL.value == entities)
    assert(rteColl.find(Rte.ref(_.int) < 10).toListL.value == entities.take(10))
  }

  test("find with projection") {
    assert(rteColl.find(projection = Rte.ref(_.int)).toListL.value == entities.map(_.int))
    assert(rteColl.find(Rte.ref(_.int) < 10, Rte.ref(_.int)).toListL.value == entities.map(_.int).take(10))

    val intWithStr = MongoProjection.zip(Rte.ref(_.int), Rte.ref(_.renamedStr))
    assert(rteColl.find(projection = intWithStr).toListL.value == entities.map(r => (r.int, r.renamedStr)))
  }

  test("find with filtering projection") {
    assert(rteColl.find(projection = Rte.ref(_.intOpt.get)).toListL.value == entities.flatMap(_.intOpt))
  }

  test("find with sort") {
    assert(rteColl.find(sort = Rte.ref(_.int).descending).toListL.value == entities.sortBy(r => -r.int))
  }
}
