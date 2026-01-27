package com.avsystem.commons
package mongo.scala

trait MongoScalaObservableExtensions {
  extension(obs: org.mongodb.scala.Observable[T]) {
    def asMonix: monix.reactive.Observable[T] = monix.reactive.Observable.fromReactivePublisher(obs)
  }
}
