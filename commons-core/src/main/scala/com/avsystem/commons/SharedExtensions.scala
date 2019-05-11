package com.avsystem.commons

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.concurrent.RunNowEC
import com.avsystem.commons.misc.{Boxing, Unboxing}
import com.github.ghik.silencer.silent

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.{AbstractIterator, mutable}
import scala.language.implicitConversions

trait SharedExtensions extends CompatSharedExtensions {
  implicit def universalOps[A](a: A): UniversalOps[A] = new UniversalOps(a)

  implicit def lazyUniversalOps[A](a: => A): LazyUniversalOps[A] = new LazyUniversalOps(() => a)

  implicit def nullableOps[A >: Null](a: A): NullableOps[A] = new NullableOps(a)

  implicit def stringOps(str: String): StringOps = new StringOps(str)

  implicit def intOps(int: Int): IntOps = new IntOps(int)

  implicit def futureOps[A](fut: Future[A]): FutureOps[A] = new FutureOps(fut)

  implicit def lazyFutureOps[A](fut: => Future[A]): LazyFutureOps[A] = new LazyFutureOps(() => fut)

  implicit def futureCompanionOps(fut: Future.type): FutureCompanionOps.type = FutureCompanionOps

  implicit def optionOps[A](option: Option[A]): OptionOps[A] = new OptionOps(option)

  implicit def tryOps[A](tr: Try[A]): TryOps[A] = new TryOps(tr)

  implicit def lazyTryOps[A](tr: => Try[A]): LazyTryOps[A] = new LazyTryOps(() => tr)

  implicit def tryCompanionOps(trc: Try.type): TryCompanionOps.type = TryCompanionOps

  implicit def partialFunctionOps[A, B](pf: PartialFunction[A, B]): PartialFunctionOps[A, B] =
    new PartialFunctionOps(pf)

  implicit def setOps[A](set: BSet[A]): SetOps[A] = new SetOps(set)

  implicit def traversableOnceOps[C[X] <: TraversableOnce[X], A](coll: C[A]): TraversableOnceOps[C, A] =
    new TraversableOnceOps(coll)

  implicit def traversableOps[C[X] <: BTraversable[X], A](coll: C[A]): TraversableOps[C, A] = new TraversableOps(coll)

  implicit def pairTraversableOnceOps[C[X] <: TraversableOnce[X], K, V](coll: C[(K, V)]): PairTraversableOnceOps[C, K, V] =
    new PairTraversableOnceOps(coll)

  implicit def mapOps[M[X, Y] <: BMap[X, Y], K, V](map: M[K, V]): MapOps[M, K, V] = new MapOps(map)

  implicit def iteratorOps[A](it: Iterator[A]): IteratorOps[A] = new IteratorOps(it)

  implicit def iteratorCompanionOps(it: Iterator.type): IteratorCompanionOps.type = IteratorCompanionOps

  implicit def orderingOps[A](ordering: Ordering[A]): OrderingOps[A] = new OrderingOps(ordering)
}

object SharedExtensions extends SharedExtensions {
  class UniversalOps[A](private val a: A) extends AnyVal {
    /**
      * The "pipe" operator. Alternative syntax to apply a function on an argument.
      * Useful for fluent expressions and avoiding intermediate variables.
      *
      * @example
      * {{{someVeryLongExpression() |> (v => if(condition(v)) something(v) else somethingElse(v))}}}
      */
    def |>[B](f: A => B): B = f(a)

    def applyIf[A0 >: A](predicate: A => Boolean)(f: A => A0): A0 =
      if (predicate(a)) f(a) else a

    /**
      * Explicit syntax to discard the value of a side-effecting expression.
      * Useful when `-Ywarn-value-discard` compiler option is enabled.
      */
    @silent
    def discard: Unit = ()

    def thenReturn[T](value: T): T = value

    def option: Option[A] = Option(a)

    def opt: Opt[A] = Opt(a)

    /**
      * Converts a boxed primitive type into an `Opt` of its corresponding primitive type, converting `null` into
      * `Opt.Empty`. For example, calling `.unboxedOpt` on a `java.lang.Integer` will convert it to `Opt[Int]`.
      */
    def unboxedOpt[B](implicit unboxing: Unboxing[B, A]): Opt[B] =
      opt.map(unboxing.fun)

    def checkNotNull(msg: String): A =
      if (a != null) a else throw new NullPointerException(msg)

    /**
      * Alternative syntax for applying some side effects on a value before returning it,
      * without having to declare an intermediate variable. Also, using `setup` confines the "setting-up"
      * code in a separate code block which has more clarity and avoids polluting outer scope.
      *
      * @example
      * {{{
      * import javax.swing._
      * // this entire expression returns the panel
      * new JPanel().setup { p =>
      *   p.setEnabled(true)
      *   p.setSize(100, 100)
      * }
      * }}}
      */
    def setup(code: A => Any): A = {
      code(a)
      a
    }

    def matchOpt[B](pf: PartialFunction[A, B]): Opt[B] =
      pf.applyOpt(a)

    /**
      * To be used instead of normal `match` keyword in pattern matching in order to suppress
      * non-exhaustive match checking.
      *
      * @example
      * {{{
      *   Option(42) uncheckedMatch {
      *     case Some(int) => println(int)
      *   }
      * }}}
      */
    def uncheckedMatch[B](pf: PartialFunction[A, B]): B =
      pf.applyOrElse(a, (obj: A) => throw new MatchError(obj))

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

    /**
      * Returns source code of the prefix expression as string, exactly as in the source file.
      * Strips common indentation. Requires -Yrangepos enabled.
      */
    def sourceCode: String = macro macros.UniversalMacros.sourceCode

    def withSourceCode: (A, String) = macro macros.UniversalMacros.withSourceCode

    def debugMacro: A = a
  }

  class LazyUniversalOps[A](private val a: () => A) extends AnyVal {
    def evalFuture: Future[A] = FutureCompanionOps.eval(a())

    def evalTry: Try[A] = Try(a())

    def optIf(condition: Boolean): Opt[A] =
      if (condition) Opt(a()) else Opt.Empty

    def optionIf(condition: Boolean): Option[A] =
      if (condition) Some(a()) else None

    def recoverFrom[T <: Throwable : ClassTag](fallbackValue: => A): A =
      try a() catch {
        case _: T => fallbackValue
      }

    def recoverToOpt[T <: Throwable : ClassTag]: Opt[A] =
      try Opt(a()) catch {
        case _: T => Opt.Empty
      }
  }

  class NullableOps[A >: Null](private val a: A) extends AnyVal {
    def optRef: OptRef[A] = OptRef(a)
  }

  private val RemovableLineBreak = "\\n+".r

  class StringOps(private val str: String) extends AnyVal {
    /**
      * Makes sure that `String` value is not `null` by replacing `null` with empty string.
      */
    def orEmpty: String =
      if (str == null) "" else str

    def ensureSuffix(suffix: String): String =
      if (str.endsWith(suffix)) str else str + suffix

    def ensurePrefix(prefix: String): String =
      if (str.startsWith(prefix)) str else prefix + str

    def uncapitalize: String =
      if (str.isEmpty || str.charAt(0).isLower) str
      else str.charAt(0).toLower + str.substring(1)

    /**
      * Removes a newline character from every sequence of consecutive newline characters. If the sequence contained
      * just one newline character without any whitespace before and after it, a space is inserted.
      *
      * e.g. `My hovercraft\nis full of eels.\n\nMy hovercraft is\n full of eels.` becomes
      * `My hovercraft is full of eels.\nMy hovercraft is full of eels.`
      *
      * Useful for multi-line string literals with lines wrapped in source code but without intention of including
      * these line breaks in actual runtime string.
      */
    def unwrapLines: String =
      RemovableLineBreak.replaceAllIn(str, { m =>
        val insertSpace = m.end == m.start + 1 && m.start - 1 >= 0 && m.end < str.length &&
          !Character.isWhitespace(str.charAt(m.start - 1)) && !Character.isWhitespace(str.charAt(m.end))
        if (insertSpace) " " else m.matched.substring(1)
      })
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

    def andThenNow[U](pf: PartialFunction[Try[A], U]): Future[A] =
      fut.andThen(pf)(RunNowEC)

    def foreachNow[U](f: A => U): Unit =
      fut.foreach(f)(RunNowEC)

    def transformNow[S](s: A => S, f: Throwable => Throwable): Future[S] =
      fut.transform(s, f)(RunNowEC)

    def transformNow[S](f: Try[A] => Try[S]): Future[S] =
      fut.transformTry(f)(RunNowEC)

    def transformWithNow[S](f: Try[A] => Future[S]): Future[S] =
      fut.transformWith(f)(RunNowEC)

    def wrapToTry: Future[Try[A]] =
      fut.transformNow(Success(_))

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

    def filterNow(p: A => Boolean): Future[A] =
      fut.filter(p)(RunNowEC)

    def collectNow[B](pf: PartialFunction[A, B]): Future[B] =
      fut.collect(pf)(RunNowEC)

    def recoverNow[U >: A](pf: PartialFunction[Throwable, U]): Future[U] =
      fut.recover(pf)(RunNowEC)

    def recoverWithNow[B >: A](pf: PartialFunction[Throwable, Future[B]]): Future[B] =
      fut.recoverWith(pf)(RunNowEC)

    def zipWithNow[B, R](that: Future[B])(f: (A, B) => R): Future[R] =
      fut.zipWith(that)(f)(RunNowEC)

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

  class LazyFutureOps[A](private val fut: () => Future[A]) extends AnyVal {
    /**
      * Evaluates a left-hand-side expression that returns a `Future` and ensures that all exceptions thrown by
      * that expression are converted to a failed `Future`.
      * Also, if left-hand-side expression returns `null`, it's converted to a `Future` failed with
      * `NullPointerException`.
      */
    def catchFailures: Future[A] = {
      val result = try fut() catch {
        case NonFatal(t) => Future.failed(t)
      }
      if (result != null) result else Future.failed(new NullPointerException("null Future"))
    }
  }

  object FutureCompanionOps {
    /**
      * Evaluates an expression and wraps its value into a `Future`. Failed `Future` is returned if expression
      * evaluation throws an exception. This is very similar to `Future.apply` but evaluates the argument immediately,
      * without dispatching it to some `ExecutionContext`.
      */
    def eval[T](expr: => T): Future[T] =
      try Future.successful(expr) catch {
        case NonFatal(cause) => Future.failed(cause)
      }

    /** Different version of `Future.traverse`. Transforms a `TraversableOnce[A]` into a `Future[TraversableOnce[B]]`,
      * which only completes after all `in` `Future`s are completed, using the provided function `A => Future[B]`.
      * This is useful for performing a parallel map. For example, to apply a function to all items of a list
      *
      * @tparam A the type of the value inside the Futures in the `TraversableOnce`
      * @tparam B the type of the value of the returned `Future`
      * @tparam M the type of the `TraversableOnce` of Futures
      * @param in the `TraversableOnce` of Futures which will be sequenced
      * @param fn the function to apply to the `TraversableOnce` of Futures to produce the results
      * @return the `Future` of the `TraversableOnce` of results
      */
    def traverseCompleted[A, B, M[X] <: TraversableOnce[X]](in: M[A])(fn: A => Future[B])(
      implicit cbf: CanBuildFrom[M[A], B, M[B]],
      executor: ExecutionContext
    ): Future[M[B]] = {
      val (barrier, i) = in.foldLeft((Future.unit, Future.successful(cbf(in)))) {
        case ((priorFinished, fr), a) =>
          val transformed = fn(a)
          (transformed.thenReturn(priorFinished), fr.zipWithNow(transformed)(_ += _))
      }
      barrier.thenReturn(i.mapNow(_.result()))
    }

    /**
      * Different version of `Future.sequence`. Transforms a `TraversableOnce[Future[A]]`
      * into a `Future[TraversableOnce[A]`, which only completes after all `in` `Future`s are completed.
      *
      * @tparam A the type of the value inside the Futures
      * @tparam M the type of the `TraversableOnce` of Futures
      * @param in the `TraversableOnce` of Futures which will be sequenced
      * @return the `Future` of the `TraversableOnce` of results
      */
    def sequenceCompleted[A, M[X] <: TraversableOnce[X]](in: M[Future[A]])(
      implicit cbf: CanBuildFrom[M[Future[A]], A, M[A]],
      executor: ExecutionContext
    ): Future[M[A]] =
      traverseCompleted(in)(identity)
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

  class LazyTryOps[A](private val tr: () => Try[A]) extends AnyVal {
    /**
      * Evaluates a left-hand side expression that return `Try`,
      * catches all exceptions and converts them into a `Failure`.
      */
    def catchFailures: Try[A] = try tr() catch {
      case NonFatal(t) => Failure(t)
    }
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
      * The same thing as `orElse` but with arguments flipped.
      * Useful in situations where `orElse` would have to be called on a partial function literal,
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

    def fold[C](a: A)(forEmpty: A => C, forNonEmpty: B => C): C = pf.applyOrElse(a, NoValueMarkerFunc) match {
      case NoValueMarker => forEmpty(a)
      case rawValue => forNonEmpty(rawValue.asInstanceOf[B])
    }
  }
  object PartialFunctionOps {
    private object NoValueMarker
    private final val NoValueMarkerFunc = (_: Any) => NoValueMarker
  }

  class TraversableOnceOps[C[X] <: TraversableOnce[X], A](private val coll: C[A]) extends AnyVal {
    def toSized[M[_]](sizeHint: Int)(implicit cbf: CanBuildFrom[Nothing, A, M[A]]): M[A] = {
      val b = cbf()
      b.sizeHint(sizeHint)
      b ++= coll
      b.result()
    }

    def toMapBy[K](keyFun: A => K): Map[K, A] =
      mkMap(keyFun, identity)

    def mkMap[K, V](keyFun: A => K, valueFun: A => V): Map[K, V] = {
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
      builders.map({ case (k, v) => (k, v.result()) })(scala.collection.breakOut)
    }

    def findOpt(p: A => Boolean): Opt[A] = coll.find(p).toOpt

    def flatCollect[B, That](f: PartialFunction[A, TraversableOnce[B]])(implicit cbf: CanBuildFrom[C[A], B, That]): That = {
      val b = cbf(coll)
      coll.foreach(f.runWith(_.foreach(b += _)))
      b.result()
    }

    def collectFirstOpt[B](pf: PartialFunction[A, B]): Opt[B] = coll.collectFirst(pf).toOpt

    def reduceOpt[A1 >: A](op: (A1, A1) => A1): Opt[A1] = if (coll.isEmpty) Opt.Empty else coll.reduce(op).opt

    def reduceLeftOpt[B >: A](op: (B, A) => B): Opt[B] = if (coll.isEmpty) Opt.Empty else coll.reduceLeft(op).opt

    def reduceRightOpt[B >: A](op: (A, B) => B): Opt[B] = if (coll.isEmpty) Opt.Empty else coll.reduceRight(op).opt

    def maxOpt[B >: A : Ordering]: Opt[B] = if (coll.isEmpty) Opt.Empty else coll.max[B].opt

    def maxOptBy[B: Ordering](f: A => B): Opt[A] = if (coll.isEmpty) Opt.Empty else coll.maxBy(f).opt

    def minOpt[B >: A : Ordering]: Opt[B] = if (coll.isEmpty) Opt.Empty else coll.min[B].opt

    def minOptBy[B: Ordering](f: A => B): Opt[A] = if (coll.isEmpty) Opt.Empty else coll.minBy(f).opt

    def mkStringOr(start: String, sep: String, end: String, default: String): String =
      if (coll.nonEmpty) coll.mkString(start, sep, end) else default

    def mkStringOr(sep: String, default: String): String =
      if (coll.nonEmpty) coll.mkString(sep) else default

    def mkStringOrEmpty(start: String, sep: String, end: String): String =
      mkStringOr(start, sep, end, "")

    def asyncFoldLeft[B](zero: Future[B])(fun: (B, A) => Future[B])(implicit ec: ExecutionContext): Future[B] =
      coll.foldLeft(zero)((fb, a) => fb.flatMap(b => fun(b, a)))

    def asyncFoldRight[B](zero: Future[B])(fun: (A, B) => Future[B])(implicit ec: ExecutionContext): Future[B] =
      coll.foldRight(zero)((a, fb) => fb.flatMap(b => fun(a, b)))

    def asyncForeach(fun: A => Future[Unit])(implicit ec: ExecutionContext): Future[Unit] =
      coll.foldLeft[Future[Unit]](Future.unit)((fu, a) => fu.flatMap(_ => fun(a)))
  }

  class PairTraversableOnceOps[C[X] <: TraversableOnce[X], K, V](private val coll: C[(K, V)]) extends AnyVal {
    def intoMap[M[X, Y] <: BMap[X, Y]](implicit cbf: CanBuildFrom[Nothing, (K, V), M[K, V]]): M[K, V] = {
      val builder = cbf()
      coll.foreach(builder += _)
      builder.result()
    }
  }

  class SetOps[A](private val set: BSet[A]) extends AnyVal {
    def containsAny(other: BTraversable[A]): Boolean = other.exists(set.contains)

    def containsAll(other: BTraversable[A]): Boolean = other.forall(set.contains)
  }

  class TraversableOps[C[X] <: BTraversable[X], A](private val coll: C[A]) extends AnyVal {
    def headOpt: Opt[A] = if (coll.isEmpty) Opt.Empty else Opt(coll.head)

    def lastOpt: Opt[A] = if (coll.isEmpty) Opt.Empty else Opt(coll.last)
  }

  class MapOps[M[X, Y] <: BMap[X, Y], K, V](private val map: M[K, V]) extends AnyVal {

    import MapOps._

    def getOpt(key: K): Opt[V] = map.get(key).toOpt

    /** For iterating, filtering, mapping etc without having to use tuples */
    def entries: Iterator[Entry[K, V]] = map.iterator.map { case (k, v) => Entry(k, v) }
  }
  object MapOps {
    case class Entry[K, V](key: K, value: V)
  }

  class IteratorOps[A](private val it: Iterator[A]) extends AnyVal {
    def pairs: Iterator[(A, A)] = new AbstractIterator[(A, A)] {
      private var first: NOpt[A] = NOpt.empty

      def hasNext: Boolean = it.hasNext && (first.nonEmpty || {
        first = NOpt(it.next())
        it.hasNext
      })

      def next(): (A, A) =
        if (!hasNext) throw new NoSuchElementException
        else {
          val f = first.get // safe because hasNext was called
          first = NOpt.Empty
          (f, it.next())
        }
    }

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

    def distinctBy[B](f: A => B): Iterator[A] =
      new AbstractIterator[A] {
        private[this] val seen = new MHashSet[B]
        private[this] var nextDistinct = NOpt.empty[A]

        @tailrec override final def hasNext: Boolean = nextDistinct.nonEmpty || it.hasNext && {
          nextDistinct = NOpt.some(it.next()).filter(a => seen.add(f(a)))
          hasNext
        }

        override def next(): A =
          if (hasNext) {
            val result = nextDistinct.get
            nextDistinct = NOpt.Empty
            result
          } else throw new NoSuchElementException
      }

    def distinct: Iterator[A] = distinctBy(identity)
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

  final class OrderingOps[A](private val ordering: Ordering[A]) extends AnyVal {
    def orElse(whenEqual: Ordering[A]): Ordering[A] =
      new Ordering[A] {
        override def compare(x: A, y: A): Int = ordering.compare(x, y) match {
          case 0 => whenEqual.compare(x, y)
          case res => res
        }
      }

    def orElseBy[B: Ordering](f: A => B): Ordering[A] = orElse(Ordering.by(f))
  }
}
