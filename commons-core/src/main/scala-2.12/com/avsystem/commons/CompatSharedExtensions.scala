package com.avsystem.commons

import com.avsystem.commons.CompatSharedExtensions.{FutureCompatOps, TryCompatOps}

trait CompatSharedExtensions {
  implicit def futureCompatOps[A](fut: Future[A]): FutureCompatOps[A] = new FutureCompatOps(fut)

  implicit def tryCompatOps[A](tr: Try[A]): TryCompatOps[A] = new TryCompatOps(tr)
}

object CompatSharedExtensions {
  final class FutureCompatOps[A](private val fut: Future[A]) extends AnyVal {
    def transformTry[S](f: Try[A] => Try[S])(implicit ec: ExecutionContext): Future[S] = fut.transform(f)
  }

  final class TryCompatOps[A](private val tr: Try[A]) extends AnyVal {
    def fold[U](ft: Throwable => U, fa: A => U): U = tr match {
      case Success(a) => fa(a)
      case Failure(t) => ft(t)
    }
  }
}
