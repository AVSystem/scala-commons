package com.avsystem.commons.misc

import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt.EmptyMarker

import scala.annotation.unchecked.uncheckedVariance
import scala.language.implicitConversions

object Opt {
  // Used as Opt's raw value to represent empty Opt. Unfortunately, null can't be used for that purpose
  // because https://github.com/scala/bug/issues/7396
  private object EmptyMarker extends Serializable

  def apply[A](value: A): Opt[A] = if (value != null) new Opt[A](value) else Opt.Empty
  def unapply[A](opt: Opt[A]): Opt[A] = opt //name-based extractor

  def some[A](value: A): Opt[A] =
    if (value != null) new Opt[A](value)
    else throw new NullPointerException

  implicit def opt2Iterable[A](xo: Opt[A]): Iterable[A] = xo.toList

  final val Empty: Opt[Nothing] = new Opt(EmptyMarker)

  def empty[A]: Opt[A] = Empty

  private val emptyMarkerFunc: Any => Any = _ => EmptyMarker

  final class WithFilter[+A] private[Opt](self: Opt[A], p: A => Boolean) {
    def map[B](f: A => B): Opt[B] = self filter p map f
    def flatMap[B](f: A => Opt[B]): Opt[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
  }
}

/**
  * Like `Option` but implemented as value class (avoids boxing) and treats `null` as no value.
  * Therefore, there is no equivalent for `Some(null)`.
  *
  * If you need a value-class version of `Option` which differentiates between no value and `null` value,
  * use [[NOpt]].
  */
final class Opt[+A] private(private val rawValue: Any) extends AnyVal with Serializable {
  private def value: A = rawValue.asInstanceOf[A]

  @inline def isEmpty: Boolean = rawValue.asInstanceOf[AnyRef] eq EmptyMarker
  @inline def isDefined: Boolean = !isEmpty
  @inline def nonEmpty: Boolean = isDefined

  @inline def get: A =
    if (isEmpty) throw new NoSuchElementException("empty Opt") else value

  @inline def boxed[B](implicit boxing: Boxing[A, B]): Opt[B] =
    map(boxing.fun)

  @inline def boxedOrNull[B >: Null](implicit boxing: Boxing[A, B]): B =
    if (isEmpty) null else boxing.fun(value)

  @inline def unboxed[B](implicit unboxing: Unboxing[B, A]): Opt[B] =
    map(unboxing.fun)

  @inline def toOption: Option[A] =
    if (isEmpty) None else Some(value)

  @inline def toOptRef[B >: Null](implicit boxing: Boxing[A, B]): OptRef[B] =
    if (isEmpty) OptRef.Empty else OptRef(boxing.fun(value))

  @inline def toNOpt: NOpt[A] =
    if (isEmpty) NOpt.Empty else NOpt(value)

  @inline def toOptArg: OptArg[A] =
    if (isEmpty) OptArg.Empty else OptArg(value)

  @inline def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else value

  @inline def orNull[B >: A](implicit ev: Null <:< B): B =
    if (isEmpty) ev(null) else value

  /**
    * Analogous to `Option.map` except that when mapping function returns `null`,
    * empty `Opt` is returned as a result.
    */
  @inline def map[B](f: A => B): Opt[B] =
    if (isEmpty) Opt.Empty else Opt(f(value))

  @inline def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  /**
    * The same as [[fold]] but takes arguments in a single parameter list for better type inference.
    */
  @inline def mapOr[B](ifEmpty: => B, f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  @inline def flatMap[B](f: A => Opt[B]): Opt[B] =
    if (isEmpty) Opt.Empty else f(value)

  @inline def flatten[B](implicit ev: A <:< Opt[B]): Opt[B] =
    if (isEmpty) Opt.Empty else ev(value)

  @inline def filter(p: A => Boolean): Opt[A] =
    if (isEmpty || p(value)) this else Opt.Empty

  @inline def withFilter(p: A => Boolean): Opt.WithFilter[A] =
    new Opt.WithFilter[A](this, p)

  @inline def filterNot(p: A => Boolean): Opt[A] =
    if (isEmpty || !p(value)) this else Opt.Empty

  @inline def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && value == elem

  @inline def exists(p: A => Boolean): Boolean =
    !isEmpty && p(value)

  @inline def forall(p: A => Boolean): Boolean =
    isEmpty || p(value)

  @inline def foreach[U](f: A => U): Unit = {
    if (!isEmpty) f(value)
  }

  /**
    * Analogous to `Option.collect` except that when the function returns `null`,
    * empty `Opt` is returned as a result.
    */
  @inline def collect[B](pf: PartialFunction[A, B]): Opt[B] =
    if (!isEmpty) {
      val res = pf.applyOrElse(value, Opt.emptyMarkerFunc)
      new Opt(if (res == null) EmptyMarker else res)
    } else Opt.Empty

  @inline def orElse[B >: A](alternative: => Opt[B]): Opt[B] =
    if (isEmpty) alternative else this

  @inline def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(value)

  @inline def toList: List[A] =
    if (isEmpty) List() else new ::(value, Nil)

  @inline def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(value)

  @inline def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(value)

  @inline def zip[B](that: Opt[B]): Opt[(A, B)] =
    if (isEmpty || that.isEmpty) Opt.Empty else Opt((this.get, that.get))

  /**
    * Apply side effect only if Opt is empty. It's a bit like foreach for Opt.Empty
    *
    * @param sideEffect - code to be executed if opt is empty
    * @return the same opt
    * @example {{{captionOpt.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
    */
  @inline def forEmpty(sideEffect: => Unit): Opt[A] = {
    if (isEmpty) {
      sideEffect
    }
    this
  }

  override def toString: String =
    if (isEmpty) "Opt.Empty" else s"Opt($value)"

}
