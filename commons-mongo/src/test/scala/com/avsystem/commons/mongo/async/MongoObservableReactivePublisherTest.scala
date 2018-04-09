package com.avsystem.commons
package mongo.async

import com.mongodb.async.client.Observer
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.mongodb.scala.{Completed, Document, FindObservable, MongoCollection, SingleObservable}
import org.scalatest.FunSuite
import org.scalatest.mockito.MockitoSugar

import scala.concurrent.duration.Duration

class MongoObservableReactivePublisherTest extends FunSuite with MockitoSugar {

  test("Dropping test collection") {
    val collection: MongoCollection[Document] = mock[MongoCollection[Document]]
    when(collection.drop()).thenReturn(SingleObservable(Completed()))
    val dropSubscriber = TestSubscriber[Completed]()

    collection.drop().asReactive.subscribe(dropSubscriber)

    dropSubscriber.assertNoTerminalEvent()
    dropSubscriber.requestMore(1)
    dropSubscriber.awaitTerminalEvent(Duration(100, "ms"))
    dropSubscriber.assertNoErrors()
    dropSubscriber.assertReceivedOnNext(Seq(Completed()))

    verify(collection).drop()
    verifyNoMoreInteractions(collection)
  }

  test("Inserting and finding documents") {
    val collection: MongoCollection[Document] = mock[MongoCollection[Document]]
    val insertSubscriber = TestSubscriber[Completed]()
    when(collection.insertMany(any())).thenReturn(SingleObservable(Completed()))

    val documents: IndexedSeq[Document] = (1 to 100) map { i: Int => Document("_id" -> i) }
    collection.insertMany(documents).asReactive.subscribe(insertSubscriber)
    insertSubscriber.requestMore(1)
    insertSubscriber.awaitTerminalEvent(Duration(10, "s"))
    insertSubscriber.assertNoErrors()
    insertSubscriber.assertReceivedOnNext(Seq(Completed()))

    verify(collection).insertMany(eqTo(documents))
    verifyNoMoreInteractions(collection)

    val original = mock[FindObservable[Document]]
    val findSubscriber = TestSubscriber[Document]()
    doNothing().when(original).subscribe(any())

    val reactive = original.asReactive
    reactive.subscribe(findSubscriber)
    findSubscriber.assertNoTerminalEvent()
    findSubscriber.requestMore(101)
    documents.foreach(findSubscriber.onNext)
    findSubscriber.onComplete()
    findSubscriber.awaitTerminalEvent(Duration(100, "ms"))
    findSubscriber.assertNoErrors()
    findSubscriber.assertReceivedOnNext(documents)


    verify(original).subscribe(any(classOf[Observer[_ >: Document]]))
    verifyNoMoreInteractions(original)
  }

}
