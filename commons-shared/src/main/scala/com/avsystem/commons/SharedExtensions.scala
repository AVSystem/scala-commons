package com.avsystem.commons

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.misc.{Boxing, NOpt, Opt, OptRef}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Success, Try}
import scala.util.control.NonFatal

/**
  * Author: ghik
  * Created: 11/01/16.
  */
trait SharedExtensions {
  implicit def universalOps[A](a: A): UniversalOps[A] = new UniversalOps(a)

  implicit def nullableOps[A >: Null](a: A): NullableOps[A] = new NullableOps(a)

  implicit def futureOps[A](fut: Future[A]): FutureOps[A] = new FutureOps(fut)

  implicit def lazyFutureOps[A](fut: => Future[A]): LazyFutureOps[A] = new LazyFutureOps(fut)

  implicit def optionOps[A](option: Option[A]): OptionOps[A] = new OptionOps(option)

  implicit def tryOps[A](tr: Try[A]): TryOps[A] = new TryOps(tr)
}

object SharedExtensions extends SharedExtensions {
  class UniversalOps[A](private val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)

    def option: Option[A] = Option(a)

    def opt: Opt[A] = Opt(a)

    def checkNotNull(msg: String): A =
      if (a != null) a else throw new NullPointerException(msg)

    def setup(code: A => Any): A = {
      code(a)
      a
    }

    /**
      * Prints AST of the prefix in a compilation error.
      * Useful for debugging macros.
      */
    def showAst: A = macro macros.UniversalMacros.showAst[A]

    /**
      * Prints raw AST of the prefix in a compilation error.
      * Useful for debugging macros.
      */
    def showRawAst: A = macro macros.UniversalMacros.showRawAst[A]

    def showSymbol: A = macro macros.UniversalMacros.showSymbol[A]

    def showSymbolFullName: A = macro macros.UniversalMacros.showSymbolFullName[A]

    def showType: A = macro macros.UniversalMacros.showType[A]

    def showRawType: A = macro macros.UniversalMacros.showRawType[A]

    def showTypeSymbol: A = macro macros.UniversalMacros.showTypeSymbol[A]

    def showTypeSymbolFullName: A = macro macros.UniversalMacros.showTypeSymbolFullName[A]
  }

  class NullableOps[A >: Null](private val a: A) extends AnyVal {
    def optRef: OptRef[A] = OptRef(a)
  }

  class FutureOps[A](private val fut: Future[A]) extends AnyVal {
    /**
      * Maps a `Future` using [[concurrent.RunNowEC RunNowEC]].
      */
    def mapNow[B](f: A => B): Future[B] =
    fut.map(f)(RunNowEC)

    /**
      * FlatMaps a `Future` using [[concurrent.RunNowEC RunNowEC]].
      */
    def flatMapNow[B](f: A => Future[B]): Future[B] =
    fut.flatMap(f)(RunNowEC)

    def toUnit: Future[Unit] =
      mapNow(_ => ())

    def toVoid: Future[Void] =
      mapNow(_ => null: Void)
  }

  class LazyFutureOps[A](fut: => Future[A]) {
    /**
      * Evaluates a left-hand-side expression that returns a `Future` and ensures that all exceptions thrown by
      * that expression are converted to a failed `Future`.
      * Also, if left-hand-side expression returns `null`, it's converted to a `Future` failed with
      * `NullPointerException`.
      */
    def catchFailures: Future[A] = {
      val result = try fut catch {
        case NonFatal(t) => Future.failed(t)
      }
      if (result != null) result else Future.failed(new NullPointerException("null Future"))
    }
  }

  class OptionOps[A](private val option: Option[A]) extends AnyVal {
    def toOpt: Opt[A] =
      if (option.isEmpty) Opt.Empty else Opt(option.get)

    def toOptRef[B >: Null](implicit boxing: Boxing[A, B]): OptRef[B] =
      if (option.isEmpty) OptRef.Empty else OptRef(boxing.fun(option.get))

    def toNOpt: NOpt[A] =
      if (option.isEmpty) NOpt.Empty else NOpt.some(option.get)
  }

  class TryOps[A](private val tr: Try[A]) extends AnyVal {
    def toOpt: Opt[A] =
      if (tr.isFailure) Opt.Empty else Opt(tr.get)

    def toOptRef[B >: Null](implicit boxing: Boxing[A, B]): OptRef[B] =
      if (tr.isFailure) OptRef.Empty else OptRef(boxing.fun(tr.get))

    def toNOpt: NOpt[A] =
      if (tr.isFailure) NOpt.Empty else NOpt.some(tr.get)
  }

  object TryOps {

    import scala.language.higherKinds

    /** Simple version of `Future.traverse`. Transforms a `TraversableOnce[Future[A]]` into a `Future[TraversableOnce[A]]`.
      *  Useful for reducing many `Future`s into a single `Future`.
      */
    def sequence[A, M[X] <: TraversableOnce[X]](in: M[Try[A]])(implicit cbf: CanBuildFrom[M[Try[A]], A, M[A]]): Try[M[A]] = {
      in.foldLeft(Try(cbf(in))) {
        (tr, tb) => {
          if (tr.isFailure && tb.isFailure) {
            tr.failed.get.addSuppressed(tb.failed.get)
            tr
          } else {
            for (r <- tr; a <- tb) yield r += a
          }
        }
      }.map(_.result())
    }

    /** Transforms a `TraversableOnce[A]` into a `Future[TraversableOnce[B]]` using the provided function `A => Future[B]`.
      * This is useful for performing a parallel map. For example, to apply a function to all items of a list
      * in parallel:
      *
      * {{{
      *    val myFutureList = Future.traverse(myList)(x => Future(myFunc(x)))
      * }}}
      */
    def traverse[A, B, M[X] <: TraversableOnce[X]](in: M[A])(fn: A => Try[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Try[M[B]] =
    in.map(fn).foldLeft(Try(cbf(in))) {
      (tr, tb) =>
        if (tr.isFailure && tb.isFailure) {
          tr.failed.get.addSuppressed(tb.failed.get)
          tr
        } else {
          for (r <- tr; b <- tb) yield r += b
        }
    }.map(_.result())
  }
}
