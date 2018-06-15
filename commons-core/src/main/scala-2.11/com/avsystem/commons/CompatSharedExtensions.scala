package com.avsystem.commons

import com.avsystem.commons.CompatSharedExtensions.FutureCompatOps
import com.avsystem.commons.concurrent.RunNowEC

trait CompatSharedExtensions {
  implicit def futureCompatOps[A](fut: Future[A]): FutureCompatOps[A] = new FutureCompatOps(fut)
}

object CompatSharedExtensions {
  final class FutureCompatOps[A](private val fut: Future[A]) extends AnyVal {
    def transformTry[S](f: Try[A] => Try[S])(implicit ec: ExecutionContext): Future[S] = {
      val p = Promise[S]
      fut.onComplete(res => p.complete(try f(res) catch {
        case NonFatal(t) => Failure(t)
      }))
      p.future
    }

    def transformWith[S](f: Try[A] => Future[S])(implicit ec: ExecutionContext): Future[S] = {
      val p = Promise[S]
      fut.onComplete(res => p.completeWith(try f(res) catch {
        case NonFatal(t) => Future.failed(t)
      }))
      p.future
    }

    def zipWith[U, R](that: Future[U])(f: (T, U) => R)(implicit executor: ExecutionContext): Future[R] =
      fut.flatMap(r1 => that.map(r2 => f(r1, r2)))(RunNowEC)
  }
}
