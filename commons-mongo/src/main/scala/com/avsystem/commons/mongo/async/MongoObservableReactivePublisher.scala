package com.avsystem.commons
package mongo.async

import com.mongodb.async.{client => mongo}
import monix.execution.atomic.AtomicBoolean
import org.{reactivestreams => reactive}

final class MongoObservableReactivePublisher[T](observable: mongo.Observable[T]) extends reactive.Publisher[T] {
  def subscribe(subscriber: reactive.Subscriber[_ >: T]): Unit = {
    observable.subscribe(
      new mongo.Observer[T]() {
        override def onSubscribe(subscription: mongo.Subscription): Unit = {
          subscriber.onSubscribe(new reactive.Subscription() {
            private final val cancelled: AtomicBoolean = AtomicBoolean(false)

            def request(n: Long): Unit = {
              if (!subscription.isUnsubscribed && n <= 0) {
                subscriber.onError(new IllegalArgumentException(
                  """3.9 While the Subscription is not cancelled,
                    |Subscription.request(long n) MUST throw a java.lang.IllegalArgumentException if the
                    |argument is <= 0.""".stripMargin
                ))
              } else {
                subscription.request(n)
              }
            }

            def cancel(): Unit = {
              if (!cancelled.getAndSet(true)) subscription.unsubscribe()
            }
          })
        }

        def onNext(result: T): Unit = subscriber.onNext(result)

        def onError(e: Throwable): Unit = subscriber.onError(e)

        def onComplete(): Unit = subscriber.onComplete()
      }
    )
  }
}
