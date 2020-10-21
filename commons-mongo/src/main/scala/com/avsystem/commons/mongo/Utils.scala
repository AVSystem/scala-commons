package com.avsystem.commons
package mongo

import monix.eval.Task
import monix.reactive.Observable
import org.reactivestreams.Publisher

object Utils {
  implicit class PublisherOps[T](private val publisher: Publisher[T]) extends AnyVal {
    def asMonix: Observable[T] = Observable.fromReactivePublisher(publisher)
    def asMonixTask: Task[T] = Observable.fromReactivePublisher(publisher).firstL
    def asMonixUnitTask: Task[Unit] = Observable.fromReactivePublisher(publisher).firstL.map(_ => ())
  }
}
