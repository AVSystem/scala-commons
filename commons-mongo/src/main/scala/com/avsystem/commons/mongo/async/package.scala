package com.avsystem.commons
package mongo

package object async {
  implicit final class MongoObservableOps[T](private val obs: com.mongodb.async.client.Observable[T]) extends AnyVal {
    def asReactive: org.reactivestreams.Publisher[T] = new MongoObservableReactivePublisher[T](obs)
    def asMonix: monix.reactive.Observable[T] = monix.reactive.Observable.fromReactivePublisher(asReactive)
  }
}
