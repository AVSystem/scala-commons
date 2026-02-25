package com.avsystem.commons
package mongo.scala

import scala.annotation.nowarn

@deprecated("Dependency on org.mongodb.scala will be removed", "2.27.0")
trait MongoScalaObservableExtensions {

  import MongoScalaObservableExtensions._

  implicit def mongoObservableOps[T](obs: org.mongodb.scala.Observable[T]): MongoObservableOps[T] =
    new MongoObservableOps[T](obs)
}

@nowarn("msg=deprecated")
object MongoScalaObservableExtensions extends MongoScalaObservableExtensions {

  final class MongoObservableOps[T](private val obs: org.mongodb.scala.Observable[T]) extends AnyVal {
    @deprecated("Dependency on org.mongodb.scala will be removed", "2.27.0")
    def asMonix: monix.reactive.Observable[T] = monix.reactive.Observable.fromReactivePublisher(obs)
  }
}
