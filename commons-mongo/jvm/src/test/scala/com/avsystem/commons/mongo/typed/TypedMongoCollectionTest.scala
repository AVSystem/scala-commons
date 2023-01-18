package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.misc.{Timestamp, TypedMap}
import com.avsystem.commons.mongo.BsonValueInput
import com.mongodb.client.model.Aggregates
import monix.eval.Task
import monix.execution.Scheduler
import org.bson.Document
import org.scalactic.source.Position
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class TypedMongoCollectionTest extends AnyFunSuite with ScalaFutures with BeforeAndAfterEach {
  implicit val scheduler: Scheduler = Scheduler.fixedPool("test", 2)

  override implicit def patienceConfig: PatienceConfig =
    PatienceConfig(timeout = Span(10, Seconds), interval = Span(100, Milliseconds))

  implicit class taskOps[T](task: Task[T]) {
    def value(implicit position: Position): T = task.runToFuture.futureValue
  }

  final val Rte = RecordTestEntity
  final val Rtaie = RecordTestAutoIdEntity

  import UnionTestEntity._

  private val client = TypedMongoClient()
  private val db = client.getDatabase("test")
  private val rteColl = db.getCollection[RecordTestEntity]("rte")
  private val rtaieColl = db.getCollection[RecordTestAutoIdEntity]("rtaie")

  private def innerRecord(i: Int): InnerRecord =
    InnerRecord(i, "istr", Opt("istropt"), Opt.Empty, List(3, 4, 5), Map("ione" -> 1, "ithree" -> 3))

  private def recordTestEntity(i: Int): RecordTestEntity = {
    val ir = innerRecord(i)

    RecordTestEntity(
      s"rid$i",
      i,
      "str",
      Timestamp.Zero,
      Opt("stropt"),
      Opt(i % 10).filter(_ % 2 == 0),
      List.range(0, i),
      Map("one" -> 1, "two" -> 2),
      TypedMap(PKey.IntKey -> i, PKey.InnerKey -> ir),
      ir,
      Opt(ir),
      List(ir),
      Map(InnerId("iid") -> ir),
      Opt(Map(InnerId("iid") -> List(ir))),
      Props(Map("foo" -> "bar")),
      i % 3 match {
        case 0 => CaseOne(s"uid$i", "ustr", i % 2 == 0)
        case 1 => CaseTwo(s"uid$i", "ustr", i, Rte.Example)
        case 2 => CaseThree(s"uid$i", "ustr", "udata", Rte.Example)
      }
    )
  }

  private val entities = (0 until 100).map(recordTestEntity)

  private[this] var seq = 100
  private def nextSeq(): Int = {
    val res = seq
    seq += 1
    res
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    Await.result(rteColl.drop().runToFuture, Duration.Inf)
    Await.result(rtaieColl.drop().runToFuture, Duration.Inf)
    Await.result(rteColl.insertMany(entities).runToFuture, Duration.Inf)
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
    assert(rteColl.find(Rte.ref(_.int) < 10).toListL.value == entities.filter(_.int < 10))
  }

  test("find with projection") {
    assert(rteColl.find(projection = Rte.ref(_.int)).toListL.value == entities.map(_.int))
    assert(rteColl.find(Rte.ref(_.int) < 10, Rte.ref(_.int)).toListL.value == entities.filter(_.int < 10).map(_.int))

    val intWithStr = MongoProjection.zip(Rte.ref(_.int), Rte.ref(_.renamedStr))
    assert(rteColl.find(projection = intWithStr).toListL.value == entities.map(r => (r.int, r.renamedStr)))
  }

  test("find with filtering projection") {
    assert(rteColl.find(projection = Rte.ref(_.intOpt.get)).toListL.value ==
      entities.flatMap(_.intOpt))
    assert(rteColl.find(projection = Rte.ref(_.union.as[CaseOne].id)).toListL.value ==
      entities.map(_.union).collect { case c1: CaseOne => c1.id })
    assert(rteColl.find(Rte.ref(_.int) < 10, projection = Rte.ref(_.union.as[CaseOne].id)).toListL.value ==
      entities.filter(_.int < 10).map(_.union).collect { case c1: CaseOne => c1.id })
  }

  test("find with sort") {
    assert(rteColl.find(sort = Rte.ref(_.int).descending).toListL.value == entities.sortBy(r => -r.int))
  }

  test("distinct") {
    assert(rteColl.distinct(Rte.ref(_.intOpt.get)).toListL.value.sorted == List(0, 2, 4, 6, 8))
  }

  test("find one and update") {
    val i = nextSeq()
    val entity = recordTestEntity(i)
    rteColl.insertOne(entity).value
    assert(rteColl.findOneAndUpdate(Rte.IdRef.is(entity.id), Rte.ref(_.int).inc(5)).value.contains(entity))
    assert(rteColl.findById(entity.id).value.contains(entity.copy(int = entity.int + 5)))
  }

  test("find one and replace") {
    val entity = recordTestEntity(nextSeq())
    val entity2 = entity.copy(int = entity.int + 5)
    rteColl.insertOne(entity).value
    assert(rteColl.findOneAndReplace(Rte.IdRef.is(entity.id), entity2).value.contains(entity))
    assert(rteColl.findById(entity.id).value.contains(entity2))
  }

  test("find one and delete") {
    val entity = recordTestEntity(nextSeq())
    rteColl.insertOne(entity).value
    assert(rteColl.findOneAndDelete(Rte.IdRef.is(entity.id)).value.contains(entity))
    assert(rteColl.findById(entity.id).value.isEmpty)
  }

  test("insert one") {
    val entity = recordTestEntity(nextSeq())
    rteColl.insertOne(entity).value
    assert(rteColl.findById(entity.id).value.contains(entity))
  }

  test("delete one") {
    val entity = recordTestEntity(nextSeq())
    rteColl.insertOne(entity).value
    assert(rteColl.deleteOne(Rte.IdRef.is(entity.id)).value.getDeletedCount == 1)
    assert(rteColl.countDocuments(Rte.IdRef.is(entity.id)).value == 0)
  }

  test("delete many") {
    val seqs = Seq.fill(10)(nextSeq())
    val entities = seqs.map(recordTestEntity)
    rteColl.insertMany(entities).value
    assert(rteColl.deleteMany(Rte.IdRef.in(entities.take(5).map(_.id))).value.getDeletedCount == 5)
    assert(rteColl.countDocuments(Rte.IdRef.in(entities.map(_.id))).value == 5)
  }

  test("update one") {
    val entity = recordTestEntity(nextSeq())
    rteColl.insertOne(entity).value
    assert(rteColl.updateOne(Rte.IdRef.is(entity.id), Rte.ref(_.int).inc(5)).value.getModifiedCount == 1)
    assert(rteColl.findById(entity.id).value.exists(_.int == entity.int + 5))
  }

  test("update one with array filters") {
    val entity = recordTestEntity(nextSeq())
    rteColl.insertOne(entity).value
    val update = Rte.ref(_.intList).updateFiltered(_ > 1, _.inc(1))
    assert(rteColl.updateOne(Rte.IdRef.is(entity.id), update).value.getModifiedCount == 1)
    assert(rteColl.findById(entity.id).value.exists(_.intList == entity.intList.map(i => if (i > 1) i + 1 else i)))
  }

  test("update many") {
    val seqs = Seq.fill(10)(nextSeq())
    val entities = seqs.map(recordTestEntity)
    rteColl.insertMany(entities).value
    val filter = Rte.IdRef.in(entities.map(_.id))
    assert(rteColl.updateMany(filter, Rte.ref(_.int).inc(5)).value.getModifiedCount == entities.size)
    assert(rteColl.find(filter, sort = Rte.ref(_.int).ascending).toListL.value == entities.map(e => e.copy(int = e.int + 5)))
  }

  test("native operation") {
    val fieldRef = "$" + Rte.ref(_.intList).rawPath
    val pipeline = JList(Aggregates.project(Bson.document("intSum", Bson.document("$sum", Bson.string(fieldRef)))))
    assert(rteColl
      .multiResultNativeOp(_.aggregate(pipeline, classOf[Document]))
      .map(_.getInteger("intSum", 0)).toListL
      .value == entities.map(_.intList.sum)
    )
  }

  test("insert auto id entity") {
    val entity = RecordTestAutoIdEntity("foo", 42)
    rtaieColl.insertOne(entity).value
    assert(rtaieColl.findOne(Rtaie.ref(_.str).is("foo")).value.contains(entity))
  }

  test("replace-upsert auto id entity") {
    val entity = RecordTestAutoIdEntity("foo", 42)
    val entity2 = RecordTestAutoIdEntity("bar", 43)
    val strIsFoo = Rtaie.ref(_.str).is("foo")
    val strIsBar = Rtaie.ref(_.str).is("bar")

    assert(rtaieColl.replaceOne(strIsFoo, entity, upsert = true).value.getMatchedCount == 0)
    assert(rtaieColl.findOne(strIsFoo).value.contains(entity))
    assert(rtaieColl.findOne(strIsBar).value.isEmpty)

    assert(rtaieColl.replaceOne(strIsFoo, entity2, upsert = true).value.getMatchedCount == 1)
    assert(rtaieColl.findOne(strIsFoo).value.isEmpty)
    assert(rtaieColl.findOne(strIsBar).value.contains(entity2))
  }

  test("read auto-id entity with id") {
    val entity = RecordTestAutoIdEntity("foo", 42)
    val id = BsonValueInput.read[TestAutoId](rtaieColl.insertOne(entity).value.getInsertedId)
    val filter = Rtaie.ref(_.str).is("foo")
    assert(rtaieColl.findOne(filter, Rtaie.IdRef zip Rtaie.SelfRef).value.contains((id, entity)))
  }

  test("successful transaction") {
    val query = Rte.IdRef.is(entities(0).id)
    val prop = Rte.ref(_.int)

    val transaction =
      client.inTransaction() { session =>
        val coll = rteColl.withSession(session)
        for {
          _ <- coll.updateOne(query, prop.inc(1))
          fromOutside <- rteColl.findOne(query, prop)
          fromInside <- coll.findOne(query, prop)
        } yield (fromOutside, fromInside)
      }

    val (fromOutside, fromInside) = transaction.value
    val committed = rteColl.findOne(query, prop).value

    // no dirty reads, old value should be visible before commit
    assert(fromOutside.contains(entities(0).int))
    // we should see the new value from within the transaction
    assert(fromInside.contains(entities(0).int + 1))
    // we should see the new value after commit
    assert(committed.contains(entities(0).int + 1))
  }

  test("failed transaction") {
    val query = Rte.IdRef.is(entities(0).id)
    val prop = Rte.ref(_.int)

    val transaction =
      client.inTransaction() { session =>
        val coll = rteColl.withSession(session)
        coll.updateOne(query, prop.inc(1)) *> Task.raiseError(new Exception)
      }

    transaction.materialize.value

    val committed = rteColl.findOne(query, prop).value
    // we should see the old value because the transaction should be aborted
    assert(committed.contains(entities(0).int))
  }
}
