package com.avsystem.commons
package mongo.async

trait MongoObservableExtensions {

  import MongoObservableExtensions._

  implicit def mongoObservableOps[T](obs: com.mongodb.async.client.Observable[T]): MongoObservableOps[T] = new MongoObservableOps[T](obs)
}

object MongoObservableExtensions extends MongoObservableExtensions {
  final class MongoObservableOps[T](private val obs: com.mongodb.async.client.Observable[T]) extends AnyVal {
    def asReactive: org.reactivestreams.Publisher[T] = new MongoObservableReactivePublisher[T](obs)
    def asMonix: monix.reactive.Observable[T] = monix.reactive.Observable.fromReactivePublisher(asReactive)
  }
}
