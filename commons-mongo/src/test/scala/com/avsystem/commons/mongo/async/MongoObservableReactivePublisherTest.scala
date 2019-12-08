package com.avsystem.commons
package mongo.async

import com.avsystem.commons.concurrent.RunNowEC
import com.github.ghik.silencer.silent
import com.mongodb.async.{client => mongo}
import monix.execution.{Cancelable, Scheduler}
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.mongodb.scala.{Completed, Document, FindObservable, MongoCollection, SingleObservable}
import org.scalactic.source.Position
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.duration.Duration
import org.scalatest.freespec.AnyFreeSpec

@silent("deprecated")
class MongoObservableReactivePublisherTest extends AnyFreeSpec with MockitoSugar {

  abstract class MockedObservableTests(implicit position: Position) extends MongoObservableExtensions {

    def subscribe[T](obs: mongo.Observable[T], testSubscriber: TestSubscriber[T]): Unit

    "should drop test collection" in {
      val collection: MongoCollection[Document] = mock[MongoCollection[Document]]
      when(collection.drop()).thenReturn(SingleObservable(Completed()))
      val dropSubscriber = TestSubscriber[Completed]()

      subscribe(collection.drop(), dropSubscriber)

      dropSubscriber.assertNoTerminalEvent()
      dropSubscriber.requestMore(1)
      dropSubscriber.awaitTerminalEvent(Duration(100, "ms"))
      dropSubscriber.assertNoErrors()
      dropSubscriber.assertReceivedOnNext(Seq(Completed()))

      verify(collection).drop()
      verifyNoMoreInteractions(collection)
    }

    "should insert documents" in {
      val collection: MongoCollection[Document] = mock[MongoCollection[Document]]
      val insertSubscriber = TestSubscriber[Completed]()
      when(collection.insertMany(any())).thenReturn(SingleObservable(Completed()))

      val documents: IndexedSeq[Document] = (1 to 100) map { i: Int => Document("_id" -> i) }
      subscribe(collection.insertMany(documents), insertSubscriber)
      insertSubscriber.requestMore(1)
      insertSubscriber.awaitTerminalEvent(Duration(100, "ms"))
      insertSubscriber.assertNoErrors()
      insertSubscriber.assertReceivedOnNext(Seq(Completed()))

      verify(collection).insertMany(eqTo(documents))
      verifyNoMoreInteractions(collection)
    }

    "should find documents" in {
      val documents: IndexedSeq[Document] = (1 to 100) map { i: Int => Document("_id" -> i) }
      val original = mock[FindObservable[Document]]
      val findSubscriber = TestSubscriber[Document]()
      doNothing().when(original).subscribe(any())

      subscribe(original, findSubscriber)
      findSubscriber.assertNoTerminalEvent()
      findSubscriber.requestMore(101)
      documents.foreach(findSubscriber.onNext)
      findSubscriber.onComplete()
      findSubscriber.awaitTerminalEvent(Duration(100, "ms"))
      findSubscriber.assertNoErrors()
      findSubscriber.assertReceivedOnNext(documents)


      verify(original).subscribe(any(classOf[mongo.Observer[_ >: Document]]))
      verifyNoMoreInteractions(original)
    }
  }

  "A Mongo-Reactive observable" - new MockedObservableTests {
    override def subscribe[T](obs: mongo.Observable[T], testSubscriber: TestSubscriber[T]): Unit =
      obs.asReactive.subscribe(testSubscriber)
  }
  "A Mongo-Monix observable" - new MockedObservableTests {
    override def subscribe[T](obs: mongo.Observable[T], testSubscriber: TestSubscriber[T]): Unit =
      obs.asMonix.subscribe(
        monix.reactive.observers.Subscriber.fromReactiveSubscriber(testSubscriber, Cancelable.empty)(Scheduler(RunNowEC))
      )
  }
}
