package com.avsystem.commons
package mongo.async

trait MongoObservableExtensions {
  import MongoObservableExtensions._

  implicit def mongoObservableOps[T](obs: org.mongodb.scala.Observable[T]): MongoObservableOps[T] = new MongoObservableOps[T](obs)
}

object MongoObservableExtensions extends MongoObservableExtensions {

  final class MongoObservableOps[T](private val obs: org.mongodb.scala.Observable[T]) extends AnyVal {
    def asMonix: monix.reactive.Observable[T] = monix.reactive.Observable.fromReactivePublisher(obs)
  }
}
