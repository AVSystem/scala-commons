package com.avsystem.commons
package rpc.akka

import monifu.concurrent.cancelables.BooleanCancelable
import monifu.reactive.Subscriber
import monifu.reactive.observables.ConnectableObservable

/**
  * @author Wojciech Milewski
  */
class RPCObservable[T] extends ConnectableObservable[T] {
  override def connect(): BooleanCancelable = ???
  override def onSubscribe(subscriber: Subscriber[T]): Unit = ???
}
