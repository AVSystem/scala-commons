package com.avsystem.commons
package mongo.reactive

import monix.eval.Task
import monix.reactive.Observable
import org.reactivestreams.Publisher

trait ReactiveMongoExtensions {
  import ReactiveMongoExtensions._

  implicit final def publisherOps[T](publisher: Publisher[T]): PublisherOps[T] = new PublisherOps(publisher)
}
object ReactiveMongoExtensions extends ReactiveMongoExtensions {
  /**
    * Extensions for converting [[Publisher]] to [[Task]]/[[Observable]] Monix types
    */
  final class PublisherOps[T](private val publisher: Publisher[T]) extends AnyVal {
    def asMonix: Observable[T] = Observable.fromReactivePublisher(publisher)

    // prefer using the family of methods below for observables which are intended to only return a single document,
    // mongo observable implementation sometimes logs an error on server-closed cursors
    private def singleObservable: Task[Option[T]] = Task.fromReactivePublisher(publisher)

    def headOptL: Task[Opt[T]] = singleObservable.map(_.toOpt)
    def headOptionL: Task[Option[T]] = singleObservable.map(_.filterNot(_ == null))
    def headL: Task[T] = singleObservable.map(_.get)
    def completedL: Task[Unit] = singleObservable.void
  }
}
