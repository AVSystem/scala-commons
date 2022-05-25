package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.macros.misc.MiscMacros
import monix.eval.Task
import monix.reactive.Observable
import org.reactivestreams.Publisher

trait TypedMongoUtils {
  protected final def empty(publisher: Publisher[Void]): Task[Unit] =
    Observable.fromReactivePublisher(publisher, 1).completedL

  protected final def single[T](publisher: Publisher[T]): Task[T] =
    Observable.fromReactivePublisher(publisher, 1).firstL

  // handles both an empty Publisher and and a single null item
  protected final def singleOpt[T](publisher: Publisher[T]): Task[Option[T]] =
    Observable.fromReactivePublisher(publisher, 1).filter(_ != null).firstOptionL

  protected final def multi[T](publisher: Publisher[T]): Observable[T] =
    Observable.fromReactivePublisher(publisher)

  protected def optionalizeFirstArg[T](expr: T): T = macro MiscMacros.optionalizeFirstArg
}
