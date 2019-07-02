package com.avsystem.commons
package misc


import com.avsystem.commons.misc.NOpt.{EmptyMarker, NullMarker}

object NOpt {
  // These two are used as NOpt's raw value to represent empty NOpt and NOpt(null).
  // Unfortunately, null itself can't be used for that purpose because https://github.com/scala/bug/issues/7396
  private object EmptyMarker extends Serializable
  private object NullMarker extends Serializable

  /**
    * Creates a [[NOpt]] out of given value. Works like `Option.apply`, i.e. `null` is translated into
    * an empty [[NOpt]]. Note however that [[NOpt]] does have a representation of "present null" (which
    * can be obtained using [[NOpt.some]]).
    */
  def apply[A](value: A): NOpt[A] =
    if (value == null) NOpt.Empty
    else new NOpt(value)

  def unapply[A](opt: NOpt[A]): NOpt[A] = opt //name-based extractor

  def some[A](value: A): NOpt[A] =
    new NOpt(if(value == null) NullMarker else value)

  implicit def opt2Iterable[A](xo: NOpt[A]): Iterable[A] = xo.toList

  final val Empty: NOpt[Nothing] = new NOpt(EmptyMarker)

  def empty[A]: NOpt[A] = Empty

  private val emptyMarkerFunc: Any => Any = _ => EmptyMarker

  final class WithFilter[+A] private[NOpt](self: NOpt[A], p: A => Boolean) {
    def map[B](f: A => B): NOpt[B] = self filter p map f
    def flatMap[B](f: A => NOpt[B]): NOpt[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
  }
}

/**
  * Like [[Opt]] but does have a counterpart for `Some(null)`. In other words, [[NOpt]] is a "nullable [[Opt]]".
  */
final class NOpt[+A] private(private val rawValue: Any) extends AnyVal with Serializable {
  private def value: A = (if (rawValue.asInstanceOf[AnyRef] eq NullMarker) null else rawValue).asInstanceOf[A]

  @inline def isEmpty: Boolean = rawValue.asInstanceOf[AnyRef] eq EmptyMarker
  @inline def isDefined: Boolean = !isEmpty
  @inline def nonEmpty: Boolean = isDefined

  @inline def get: A =
    if (isEmpty) throw new NoSuchElementException("empty NOpt") else value

  @inline def boxed[B](implicit boxing: Boxing[A, B]): NOpt[B] =
    map(boxing.fun)

  @inline def unboxed[B](implicit unboxing: Unboxing[B, A]): NOpt[B] =
    map(unboxing.fun)

  @inline def toOption: Option[A] =
    if (isEmpty) None else Some(value)

  /**
    * Converts this `NOpt` into `Opt`. Because `Opt` cannot hold `null`, `NOpt(null)` is translated to `Opt.Empty`.
    */
  @inline def toOpt: Opt[A] =
    if (isEmpty) Opt.Empty else Opt(value)

  /**
    * Converts this `NOpt` into `OptRef`, changing the element type into boxed representation if
    * necessary (e.g. `Boolean` into `java.lang.Boolean`). Because `OptRef` cannot hold `null`,
    * `NOpt(null)` is translated to `OptRef.Empty`.
    */
  @inline def toOptRef[B >: Null](implicit boxing: Boxing[A, B]): OptRef[B] =
    if (isEmpty) OptRef.Empty else OptRef(boxing.fun(value))

  /**
    * Converts this `NOpt` into `OptArg`. Because `OptArg` cannot hold `null`, `NOpt(null)` is translated to `OptArg.Empty`.
    */
  @inline def toOptArg: OptArg[A] =
    if (isEmpty) OptArg.Empty else OptArg(value)

  @inline def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else value

  @inline def orNull[B >: A](implicit ev: Null <:< B): B =
    if (isEmpty) ev(null) else value

  @inline def map[B](f: A => B): NOpt[B] =
    if (isEmpty) NOpt.Empty else NOpt.some(f(value))

  @inline def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  /**
    * The same as [[fold]] but takes arguments in a single parameter list for better type inference.
    */
  @inline def mapOr[B](ifEmpty: => B, f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  @inline def flatMap[B](f: A => NOpt[B]): NOpt[B] =
    if (isEmpty) NOpt.Empty else f(value)

  @inline def flatten[B](implicit ev: A <:< NOpt[B]): NOpt[B] =
    if (isEmpty) NOpt.Empty else ev(value)

  @inline def filter(p: A => Boolean): NOpt[A] =
    if (isEmpty || p(value)) this else NOpt.Empty

  @inline def withFilter(p: A => Boolean): NOpt.WithFilter[A] =
    new NOpt.WithFilter[A](this, p)

  @inline def filterNot(p: A => Boolean): NOpt[A] =
    if (isEmpty || !p(value)) this else NOpt.Empty

  @inline def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && value == elem

  @inline def exists(p: A => Boolean): Boolean =
    !isEmpty && p(value)

  @inline def forall(p: A => Boolean): Boolean =
    isEmpty || p(value)

  @inline def foreach[U](f: A => U): Unit = {
    if (!isEmpty) f(value)
  }

  @inline def collect[B](pf: PartialFunction[A, B]): NOpt[B] =
    if (!isEmpty) {
      val res = pf.applyOrElse(value, NOpt.emptyMarkerFunc)
      new NOpt(if (res == null) NullMarker else res)
    } else NOpt.Empty

  @inline def orElse[B >: A](alternative: => NOpt[B]): NOpt[B] =
    if (isEmpty) alternative else this

  @inline def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(value)

  @inline def toList: List[A] =
    if (isEmpty) List() else new ::(value, Nil)

  @inline def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(value)

  @inline def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(value)

  @inline def zip[B](that: NOpt[B]): NOpt[(A, B)] =
    if (isEmpty || that.isEmpty) NOpt.Empty else NOpt((this.get, that.get))

  /**
    * Apply side effect only if NOpt is empty. It's a bit like foreach for NOpt.Empty
    * @param sideEffect - code to be executed if nopt is empty
    * @return the same nopt
    * @example {{{captionNOpt.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
    */
  @inline def forEmpty(sideEffect: => Unit): NOpt[A] = {
    if (isEmpty) {
      sideEffect
    }
    this
  }

  override def toString: String =
    if (isEmpty) "NOpt.Empty" else s"NOpt($value)"
}
