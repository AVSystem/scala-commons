package com.avsystem.commons
package mongo.async

import com.github.ghik.silencer.silent

trait MongoObservableExtensions {

  import MongoObservableExtensions._

  @silent("deprecated")
  implicit def mongoObservableOps[T](obs: com.mongodb.async.client.Observable[T]): MongoObservableOps[T] = new MongoObservableOps[T](obs)
}

object MongoObservableExtensions extends MongoObservableExtensions {
  @silent("deprecated")
  final class MongoObservableOps[T](private val obs: com.mongodb.async.client.Observable[T]) extends AnyVal {
    def asReactive: org.reactivestreams.Publisher[T] = new MongoObservableReactivePublisher[T](obs)
  }
}
