package com.avsystem.commons

import com.avsystem.commons.CompatSharedExtensions.FutureCompatOps
import com.avsystem.commons.concurrent.RunNowEC

trait CompatSharedExtensions {
  implicit def futureCompatOps[A](fut: Future[A]): FutureCompatOps[A] = new FutureCompatOps(fut)
}

object CompatSharedExtensions {
  final class FutureCompatOps[A](private val fut: Future[A]) extends AnyVal {
    def transformTry[S](f: Try[A] => Try[S])(implicit ec: ExecutionContext): Future[S] = fut.transform(f)

    def zipWithNow[B, R](that: Future[B])(f: (A, B) => R): Future[R] =
      fut.zipWith(that)(f)(RunNowEC)
  }
}
