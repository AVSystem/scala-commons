package com.avsystem.commons

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.misc.{Boxing, Unboxing}

import scala.collection.generic.CanBuildFrom
import scala.collection.{AbstractIterator, mutable}
import scala.language.implicitConversions

trait SharedExtensions {
  implicit def universalOps[A](a: A): UniversalOps[A] = new UniversalOps(a)

  implicit def lazyUniversalOps[A](a: => A): LazyUniversalOps[A] = new LazyUniversalOps(() => a)

  implicit def nullableOps[A >: Null](a: A): NullableOps[A] = new NullableOps(a)

  implicit def intOps(int: Int): IntOps = new IntOps(int)

  implicit def futureOps[A](fut: Future[A]): FutureOps[A] = new FutureOps(fut)

  implicit def lazyFutureOps[A](fut: => Future[A]): LazyFutureOps[A] = new LazyFutureOps(fut)

  implicit def futureCompanionOps(fut: Future.type): FutureCompanionOps.type = FutureCompanionOps

  implicit def optionOps[A](option: Option[A]): OptionOps[A] = new OptionOps(option)

  implicit def tryOps[A](tr: Try[A]): TryOps[A] = new TryOps(tr)

  implicit def tryCompanionOps(trc: Try.type): TryCompanionOps.type = TryCompanionOps

  implicit def partialFunctionOps[A, B](pf: PartialFunction[A, B]): PartialFunctionOps[A, B] = new PartialFunctionOps(pf)

  implicit def traversableOps[C[X] <: BTraversable[X], A](coll: C[A]): TraversableOps[C, A] = new TraversableOps(coll)

  implicit def setOps[A](set: BSet[A]): SetOps[A] = new SetOps(set)

  implicit def mapOps[M[X, Y] <: BMap[X, Y], K, V](map: M[K, V]): MapOps[M, K, V] = new MapOps(map)

  implicit def iteratorOps[A](it: Iterator[A]): IteratorOps[A] = new IteratorOps(it)

  implicit def iteratorCompanionOps(it: Iterator.type): IteratorCompanionOps.type = IteratorCompanionOps
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

    def matchOpt[B](pf: PartialFunction[A, B]): Opt[B] =
      pf.applyOpt(a)

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

  class LazyUniversalOps[A](private val a: () => A) extends AnyVal {
    def evalFuture: Future[A] = FutureCompanionOps.eval(a())

    def evalTry: Try[A] = Try(a())
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
    def onCompleteNow[U](f: Try[A] => U): Unit =
      fut.onComplete(f)(RunNowEC)

    def andThenNow[U](pf: PartialFunction[Try[A], U]): Unit =
      fut.andThen(pf)(RunNowEC)

    def foreachNow[U](f: A => U): Unit =
      fut.foreach(f)(RunNowEC)

    def transformNow[S](s: A => S, f: Throwable => Throwable): Future[S] =
      fut.transform(s, f)(RunNowEC)

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

    def recoverNow[U >: A](pf: PartialFunction[Throwable, U]): Future[U] =
      fut.recover(pf)(RunNowEC)

    def recoverWithNow[B >: A](pf: PartialFunction[Throwable, Future[B]]): Future[B] =
      fut.recoverWith(pf)(RunNowEC)

    def toUnit: Future[Unit] =
      mapNow(_ => ())

    def toVoid: Future[Void] =
      mapNow(_ => null: Void)

    /**
      * Returns a `Future` that completes with the specified `result`, but only after this future completes.
      */
    def thenReturn[T](result: Future[T]): Future[T] = {
      val p = Promise[T]()
      fut.onComplete(_ => p.completeWith(result))(RunNowEC)
      p.future
    }

    /**
      * Returns a `Future` that completes successfully, but only after this future completes.
      */
    def ignoreFailures: Future[Unit] =
      thenReturn(Future.successful {})
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

  object FutureCompanionOps {
    def eval[T](expr: => T): Future[T] =
      try Future.successful(expr) catch {
        case NonFatal(cause) => Future.failed(cause)
      }
  }

  class OptionOps[A](private val option: Option[A]) extends AnyVal {
    /**
      * Converts this `Option` into `Opt`. Because `Opt` cannot hold `null`, `Some(null)` is translated to `Opt.Empty`.
      */
    def toOpt: Opt[A] =
      if (option.isEmpty) Opt.Empty else Opt(option.get)

    /**
      * Converts this `Option` into `OptRef`, changing the element type into boxed representation if
      * necessary (e.g. `Boolean` into `java.lang.Boolean`). Because `OptRef` cannot hold `null`,
      * `Some(null)` is translated to `OptRef.Empty`.
      */
    def toOptRef[B >: Null](implicit boxing: Boxing[A, B]): OptRef[B] =
      if (option.isEmpty) OptRef.Empty else OptRef(boxing.fun(option.get))

    def toNOpt: NOpt[A] =
      if (option.isEmpty) NOpt.Empty else NOpt.some(option.get)

    /**
      * Converts this `Option` into `OptArg`. Because `OptArg` cannot hold `null`, `Some(null)` is translated to `OptArg.Empty`.
      */
    def toOptArg: OptArg[A] =
      if (option.isEmpty) OptArg.Empty else OptArg(option.get)

    /**
      * Apply side effect only if Option is empty. It's a bit like foreach for None
      *
      * @param sideEffect - code to be executed if option is empty
      * @return the same option
      * @example {{{captionOpt.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
      */
    def forEmpty(sideEffect: => Unit): Option[A] = {
      if (option.isEmpty) {
        sideEffect
      }
      option
    }
  }

  class TryOps[A](private val tr: Try[A]) extends AnyVal {
    /**
      * Converts this `Try` into `Opt`. Because `Opt` cannot hold `null`, `Success(null)` is translated to `Opt.Empty`.
      */
    def toOpt: Opt[A] =
      if (tr.isFailure) Opt.Empty else Opt(tr.get)

    /**
      * Converts this `Try` into `OptRef`, changing the element type into boxed representation if
      * necessary (e.g. `Boolean` into `java.lang.Boolean`). Because `OptRef` cannot hold `null`,
      * `Success(null)` is translated to `OptRef.Empty`.
      */
    def toOptRef[B >: Null](implicit boxing: Boxing[A, B]): OptRef[B] =
      if (tr.isFailure) OptRef.Empty else OptRef(boxing.fun(tr.get))

    def toNOpt: NOpt[A] =
      if (tr.isFailure) NOpt.Empty else NOpt.some(tr.get)

    /**
      * Converts this `Try` into `OptArg`. Because `OptArg` cannot hold `null`, `Success(null)` is translated to `OptArg.Empty`.
      */
    def toOptArg: OptArg[A] =
      if (tr.isFailure) OptArg.Empty else OptArg(tr.get)
  }

  object TryCompanionOps {

    import scala.language.higherKinds

    /** Simple version of `TryOps.traverse`. Transforms a `TraversableOnce[Try[A]]` into a `Try[TraversableOnce[A]]`.
      * Useful for reducing many `Try`s into a single `Try`.
      */
    def sequence[A, M[X] <: TraversableOnce[X]](in: M[Try[A]])(implicit cbf: CanBuildFrom[M[Try[A]], A, M[A]]): Try[M[A]] = {
      in.foldLeft(Try(cbf(in))) {
        case (f@Failure(e), Failure(newEx)) => e.addSuppressed(newEx); f
        case (tr, tb) => for (r <- tr; a <- tb) yield r += a
      }.map(_.result())
    }

    /** Transforms a `TraversableOnce[A]` into a `Try[TraversableOnce[B]]` using the provided function `A => Try[B]`.
      * For example, to apply a function to all items of a list:
      *
      * {{{
      *    val myTryList = TryOps.traverse(myList)(x => Try(myFunc(x)))
      * }}}
      */
    def traverse[A, B, M[X] <: TraversableOnce[X]](in: M[A])(fn: A => Try[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Try[M[B]] =
      in.map(fn).foldLeft(Try(cbf(in))) {
        case (f@Failure(e), Failure(newEx)) => e.addSuppressed(newEx); f
        case (tr, tb) => for (r <- tr; b <- tb) yield r += b
      }.map(_.result())

  }

  class PartialFunctionOps[A, B](private val pf: PartialFunction[A, B]) extends AnyVal {

    import PartialFunctionOps._

    /**
      * The same thing as [[scala.PartialFunction.orElse]] but with arguments flipped.
      * Useful in situations where [[scala.PartialFunction.orElse]] would have to be called on a partial function literal,
      * which does not work well with type inference.
      */
    def unless(pre: PartialFunction[A, B]): PartialFunction[A, B] = pre orElse pf

    def applyNOpt(a: A): NOpt[B] = pf.applyOrElse(a, NoValueMarkerFunc) match {
      case NoValueMarker => NOpt.Empty
      case rawValue => NOpt.some(rawValue.asInstanceOf[B])
    }

    def applyOpt(a: A): Opt[B] = pf.applyOrElse(a, NoValueMarkerFunc) match {
      case NoValueMarker => Opt.Empty
      case rawValue => Opt(rawValue.asInstanceOf[B])
    }
  }
  object PartialFunctionOps {
    private object NoValueMarker
    private final val NoValueMarkerFunc = (_: Any) => NoValueMarker
  }

  class TraversableOps[C[X] <: BTraversable[X], A](private val coll: C[A]) extends AnyVal {
    def mkMap[K, V](keyFun: A => K, valueFun: A => V): Map[K, V] = {
      coll.map { a => (keyFun(a), valueFun(a)) }(scala.collection.breakOut)
    }

    def groupToMap[K, V, To](keyFun: A => K, valueFun: A => V)(implicit cbf: CanBuildFrom[C[A], V, To]): Map[K, To] = {
      val builders = mutable.Map[K, mutable.Builder[V, To]]()
      coll.foreach { a =>
        builders.getOrElseUpdate(keyFun(a), cbf(coll)) += valueFun(a)
      }
      builders.map({ case (k, v) => (k, v.result()) })(scala.collection.breakOut)
    }

    def headOpt: Opt[A] = if (coll.isEmpty) Opt.Empty else Opt(coll.head)

    def lastOpt: Opt[A] = if (coll.isEmpty) Opt.Empty else Opt(coll.last)

    def findOpt(p: A => Boolean): Opt[A] = coll.find(p).toOpt

    def collectFirstOpt[B](pf: PartialFunction[A, B]): Opt[B] = coll.collectFirst(pf).toOpt

    def reduceOpt[A1 >: A](op: (A1, A1) => A1): Opt[A1] = if (coll.isEmpty) Opt.Empty else coll.reduce(op).opt

    def reduceLeftOpt[B >: A](op: (B, A) => B): Opt[B] = if (coll.isEmpty) Opt.Empty else coll.reduceLeft(op).opt

    def reduceRightOpt[B >: A](op: (A, B) => B): Opt[B] = if (coll.isEmpty) Opt.Empty else coll.reduceRight(op).opt

    def maxOpt[B >: A : Ordering]: Opt[B] = if (coll.isEmpty) Opt.Empty else coll.max[B].opt

    def maxOptBy[B: Ordering](f: A => B): Opt[A] = if (coll.isEmpty) Opt.Empty else coll.maxBy(f).opt

    def minOpt[B >: A : Ordering]: Opt[B] = if (coll.isEmpty) Opt.Empty else coll.min[B].opt

    def minOptBy[B: Ordering](f: A => B): Opt[A] = if (coll.isEmpty) Opt.Empty else coll.minBy(f).opt
  }

  class SetOps[A](private val set: BSet[A]) extends AnyVal {
    def containsAny(other: BTraversable[A]): Boolean = other.exists(set.contains)

    def containsAll(other: BTraversable[A]): Boolean = other.forall(set.contains)
  }

  class MapOps[M[X, Y] <: BMap[X, Y], K, V](private val map: M[K, V]) extends AnyVal {
    def getOpt(key: K): Opt[V] = map.get(key).toOpt
  }

  class IteratorOps[A](private val it: Iterator[A]) extends AnyVal {
    def nextOpt: Opt[A] =
      if (it.hasNext) it.next().opt else Opt.Empty

    def drainTo[C[_]](n: Int)(implicit cbf: CanBuildFrom[Nothing, A, C[A]]): C[A] = {
      val builder = cbf()
      var i = 0
      while (it.hasNext && i < n) {
        builder += it.next()
        i += 1
      }
      builder.result()
    }

    def collectWhileDefined[B](pf: PartialFunction[A, B]): Iterator[B] =
      new AbstractIterator[B] {
        private[this] var fetched = false
        private[this] var value: NOpt[B] = _

        private[this] def fetch(): Unit =
          if (it.hasNext) {
            value = pf.applyNOpt(it.next())
          } else {
            value = NOpt.Empty
          }

        def hasNext: Boolean = {
          if (!fetched) {
            fetch()
            fetched = true
          }
          value.isDefined
        }

        def next(): B = {
          if (!fetched) {
            fetch()
          }
          value match {
            case NOpt(v) =>
              fetched = false
              v
            case NOpt.Empty =>
              throw new NoSuchElementException
          }
        }
      }
  }

  object IteratorCompanionOps {
    def untilEmpty[T](elem: => Opt[T]): Iterator[T] =
      new AbstractIterator[T] {
        private[this] var fetched = false
        private[this] var value = Opt.empty[T]

        def hasNext = {
          if (!fetched) {
            value = elem
            fetched = true
          }
          value.isDefined
        }

        def next = {
          if (!fetched) {
            value = elem
          }
          value match {
            case Opt(v) =>
              fetched = false
              v
            case Opt.Empty =>
              throw new NoSuchElementException
          }
        }
      }

    def iterateUntilEmpty[T](start: Opt[T])(nextFun: T => Opt[T]): Iterator[T] =
      new AbstractIterator[T] {
        private[this] var fetched = true
        private[this] var value = start

        def hasNext = {
          if (!fetched) {
            value = nextFun(value.get)
            fetched = true
          }
          value.isDefined
        }

        def next = {
          if (!fetched) {
            value = nextFun(value.get)
          }
          value match {
            case Opt(v) =>
              fetched = false
              v
            case Opt.Empty =>
              throw new NoSuchElementException
          }
        }
      }
  }
}
