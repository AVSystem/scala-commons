package com.avsystem.commons
package concurrent

import com.avsystem.commons.concurrent.ObservableExtensions.ObservableOps
import monix.eval.Task
import monix.reactive.Observable

import scala.util.Sorting

trait ObservableExtensions {
  implicit final def observableOps[T](obs: Observable[T]): ObservableOps[T] = new ObservableOps(obs)
}

object ObservableExtensions extends ObservableExtensions {
  final class ObservableOps[T](private val obs: Observable[T]) extends AnyVal {

    import scala.collection.compat._

    def headOptL: Task[Opt[T]] = obs.headOptionL.map(_.toOpt)

    def distinct: Observable[T] = distinctBy[T](identity)

    def distinctBy[K](key: T => K): Observable[T] =
      obs
        .scan((MSet.empty[K], Opt.empty[T])) { case ((seenElems, _), elem) =>
          (seenElems, elem.optIf(seenElems.add(key(elem))))
        }
        .collect { case (_, Opt(elem)) => elem }

    def sortedL(implicit ord: Ordering[T], ct: ClassTag[T]): Task[ISeq[T]] =
      obs
        .toL(Array)
        .map { arr =>
          Sorting.stableSort(arr)
          immutable.ArraySeq.unsafeWrapArray(arr)
        }

    def sortedByL[R](f: T => R)(implicit ord: Ordering[R], ct: ClassTag[T]): Task[ISeq[T]] =
      sortedL(ord on f, ct)

    def toL[R](factory: Factory[T, R]): Task[R] =
      obs
        .foldLeftL(factory.newBuilder)(_ += _)
        .map(_.result())
  }
}
