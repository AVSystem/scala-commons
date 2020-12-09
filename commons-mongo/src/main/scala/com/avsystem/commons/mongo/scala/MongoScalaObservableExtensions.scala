package com.avsystem.commons
package mongo.scala

trait MongoScalaObservableExtensions {
  import MongoScalaObservableExtensions._

  implicit def mongoObservableOps[T](obs: org.mongodb.scala.Observable[T]): MongoObservableOps[T] = new MongoObservableOps[T](obs)
}

object MongoScalaObservableExtensions extends MongoScalaObservableExtensions {

  final class MongoObservableOps[T](private val obs: org.mongodb.scala.Observable[T]) extends AnyVal {
    def asMonix: monix.reactive.Observable[T] = monix.reactive.Observable.fromReactivePublisher(obs)
  }
}
