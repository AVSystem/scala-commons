package com.avsystem.commons

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.collection.CollectionAliases.{BMap, BTraversable}
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.misc.{Boxing, NOpt, Opt, OptRef, Unboxing}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Try
import scala.util.control.NonFatal

/**
  * Author: ghik
  * Created: 11/01/16.
  */
trait SharedExtensions {
  implicit def universalOps[A](a: A): UniversalOps[A] = new UniversalOps(a)

  implicit def nullableOps[A >: Null](a: A): NullableOps[A] = new NullableOps(a)

  implicit def intOps(int: Int): IntOps = new IntOps(int)

  implicit def futureOps[A](fut: Future[A]): FutureOps[A] = new FutureOps(fut)

  implicit def lazyFutureOps[A](fut: => Future[A]): LazyFutureOps[A] = new LazyFutureOps(fut)

  implicit def optionOps[A](option: Option[A]): OptionOps[A] = new OptionOps(option)

  implicit def tryOps[A](tr: Try[A]): TryOps[A] = new TryOps(tr)

  implicit def collectionOps[C[X] <: BTraversable[X], A](coll: C[A]): CollectionOps[C, A] = new CollectionOps(coll)

  implicit def mapOps[M[X, Y] <: BMap[X, Y], K, V](map: M[K, V]): MapOps[M, K, V] = new MapOps(map)
}

object SharedExtensions extends SharedExtensions {
  class UniversalOps[A](private val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)

    def option: Option[A] = Option(a)

    def opt: Opt[A] = Opt(a)

    def unboxedOpt[B](implicit unboxing: Unboxing[B, A]): Opt[B] =
      opt.map(unboxing.fun)

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

  class IntOps(private val int: Int) extends AnyVal {
    def times(code: => Any): Unit = {
      var i = 0
      while (i < int) {
        code
        i += 1
      }
    }
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

    def recoverWithNow[B >: A](pf: PartialFunction[Throwable, Future[B]]): Future[B] =
      fut.recoverWith(pf)(RunNowEC)

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

  class CollectionOps[C[X] <: BTraversable[X], A](private val coll: C[A]) extends AnyVal {
    def toMap[K, V](keyFun: A => K, valueFun: A => V): Map[K, V] = {
      val res = Map.newBuilder[K, V]
      coll.foreach { a =>
        res += ((keyFun(a), valueFun(a)))
      }
      res.result()
    }

    def groupToMap[K, V, To](keyFun: A => K, valueFun: A => V)(implicit cbf: CanBuildFrom[C[A], V, To]): Map[K, To] = {
      val builders = mutable.Map[K, mutable.Builder[V, To]]()
      coll.foreach { a =>
        builders.getOrElseUpdate(keyFun(a), cbf(coll)) += valueFun(a)
      }
      builders.iterator.map({ case (k, v) => (k, v.result()) }).toMap
    }

    def headOpt: Opt[A] = coll.headOption.toOpt

    def lastOpt: Opt[A] = coll.lastOption.toOpt

    def findOpt(p: A => Boolean) = coll.find(p).toOpt

    def collectFirstOpt[B](pf: PartialFunction[A, B]): Opt[B] = coll.collectFirst(pf).toOpt

    def reduceOpt[A1 >: A](op: (A1, A1) => A1): Opt[A1] = coll.reduceOption(op).toOpt

    def reduceLeftOpt[B >: A](op: (B, A) => B): Opt[B] = coll.reduceLeftOption(op).toOpt

    def reduceRightOpt[B >: A](op: (A, B) => B): Opt[B] = coll.reduceRightOption(op).toOpt

    def maxOpt[B >: A](implicit cmp: Ordering[B]): Opt[B] = if (coll.isEmpty) Opt.Empty else coll.max[B].opt

    def maxOptBy[B](f: A => B)(implicit cmp: Ordering[B]): Opt[A] = if (coll.isEmpty) Opt.Empty else coll.maxBy(f).opt

    def minOpt[B >: A](implicit cmp: Ordering[B]): Opt[B] = if (coll.isEmpty) Opt.Empty else coll.min[B].opt

    def minOptBy[B](f: A => B)(implicit cmp: Ordering[B]): Opt[A] = if (coll.isEmpty) Opt.Empty else coll.minBy(f).opt
  }

  class MapOps[M[X, Y] <: BMap[X, Y], K, V](private val map: M[K, V]) extends AnyVal {
    def getOpt(key: K): Opt[V] = map.get(key).toOpt
  }
}
