package com.avsystem.commons

import com.avsystem.commons.concurrent.RunNowEC

trait CompatSharedExtensions {

  import CompatSharedExtensions._

  implicit def futureCompatOps[A](fut: Future[A]): FutureCompatOps[A] = new FutureCompatOps(fut)

  implicit def futureCompanionCompatOps(fut: Future.type): FutureCompanionCompatOps.type = FutureCompanionCompatOps

  implicit def tryCompatOps[A](tr: Try[A]): TryCompatOps[A] = new TryCompatOps(tr)
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

    def zipWith[U, R](that: Future[U])(f: (A, U) => R)(implicit executor: ExecutionContext): Future[R] =
      fut.flatMap(r1 => that.map(r2 => f(r1, r2)))(RunNowEC)
  }

  object FutureCompanionCompatOps {
    final val unit: Future[Unit] = Future.successful(())
  }

  final class TryCompatOps[A](private val tr: Try[A]) extends AnyVal {
    def fold[U](ft: Throwable => U, fa: A => U): U = tr match {
      case Success(a) => fa(a)
      case Failure(t) => ft(t)
    }
  }
}
