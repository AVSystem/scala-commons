package com.avsystem.commons.misc

import com.avsystem.commons.IIterable

object OptRef {
  def apply[A](value: A | Null): OptRef[A] = new OptRef[A](value)
  def unapply[A](opt: OptRef[A]): OptRef[A] = opt // name-based extractor

  def some[A](value: A | Null): OptRef[A] =
    if (value != null) new OptRef[A](value)
    else throw new NullPointerException

  object Boxed {
    def unapply[A, B >: Null](optRef: OptRef[B])(implicit unboxing: Unboxing[A, B]): Opt[A] =
      if (optRef.isEmpty) Opt.Empty else Opt(unboxing.fun(optRef.get))
  }

  implicit def opt2Iterable[A](xo: OptRef[A]): IIterable[A] = xo.toList

  final val Empty: OptRef[Nothing] = new OptRef[Nothing](null.asInstanceOf[Nothing])
  def empty[A]: OptRef[A] = Empty

  private val nullFunc: Any => Null = _ => null

  final class WithFilter[+A] private[OptRef] (self: OptRef[A], p: A => Boolean) {
    def map[B](f: A => B): OptRef[B] = self.filter(p).`map`(f)
    def flatMap[B](f: A => OptRef[B]): OptRef[B] = self.filter(p).`flatMap`(f)
    def foreach[U](f: A => U): Unit = self.filter(p).foreach(f)
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
  }
}

/**
 * Like [[Opt]] but has better Java interop thanks to the fact that wrapped value has type `A` instead of `Any`. For
 * example, Scala method defined like this:
 * {{{
 *   def takeMaybeString(str: OptRef[String]): Unit
 * }}}
 * will be seen by Java as:
 * {{{
 *   public void takeMaybeString(String str);
 * }}}
 * and `null` will be used to represent absence of value. <p/> This comes at the cost of `A` having to be a nullable
 * type. Also, empty value is represented internally using `null` which unfortunately makes [[OptRef]] suffer from
 * SI-7396 (`hashCode` fails on `OptRef.Empty` which means that you can't add [[OptRef]] values into hash sets or use
 * them as hash map keys).
 */
final class OptRef[+A] private (private val value: A | Null) extends AnyVal with OptBase[A] with Serializable {
  @inline def isEmpty: Boolean = value == null
  @inline def isDefined: Boolean = !isEmpty
  @inline def nonEmpty: Boolean = isDefined

  @inline def get: A =
    if (isEmpty) throw new NoSuchElementException("empty OptRef") else value.nn

  @inline def toOpt: Opt[A] =
    Opt(value)

  @inline def toOption: Option[A] =
    Option(value)

  @inline def toNOpt: NOpt[A] =
    if (isEmpty) NOpt.Empty else NOpt(value)

  @inline def toOptArg: OptArg[A] =
    if (isEmpty) OptArg.Empty else OptArg(value)

  @inline def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else value.nn

  @inline def orNull[B >: A](implicit ev: Null <:< B): B =
    value.asInstanceOf[B]

  @inline def map[B](f: A => B | Null): OptRef[B] =
    if (isEmpty) OptRef.Empty else OptRef(f(value.nn))

  @inline def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(value.nn)

  /**
   * The same as [[fold]] but takes arguments in a single parameter list for better type inference.
   */
  @inline def mapOr[B](ifEmpty: => B, f: A => B): B =
    if (isEmpty) ifEmpty else f(value.nn)

  @inline def flatMap[B](f: A => OptRef[B]): OptRef[B] =
    if (isEmpty) OptRef.Empty else f(value.nn)

  @inline def filter(p: A => Boolean): OptRef[A] =
    if (isEmpty || p(value.nn)) this else OptRef.Empty

  @inline def withFilter(p: A => Boolean): OptRef.WithFilter[A] =
    new OptRef.WithFilter[A](this, p)

  @inline def filterNot(p: A => Boolean): OptRef[A] =
    if (isEmpty || !p(value.nn)) this else OptRef.Empty

  @inline def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && value.nn == elem

  @inline def exists(p: A => Boolean): Boolean =
    !isEmpty && p(value.nn)

  @inline def forall(p: A => Boolean): Boolean =
    isEmpty || p(value.nn)

  @inline def foreach[U](f: A => U): Unit = {
    if (!isEmpty) f(value.nn)
  }

  @inline def collect[B](pf: PartialFunction[A, B]): OptRef[B] =
    if (!isEmpty) new OptRef(pf.applyOrElse(value.nn, OptRef.nullFunc)) else OptRef.Empty

  @inline def orElse[B >: A](alternative: => OptRef[B]): OptRef[B] =
    if (isEmpty) alternative else this

  @inline def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(value.nn)

  @inline def toList: List[A] =
    if (isEmpty) List() else new ::(value.nn, Nil)

  @inline def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(value.nn)

  @inline def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(value.nn)

  @inline def zip[B](that: OptRef[B]): OptRef[(A, B)] =
    if (isEmpty || that.isEmpty) OptRef.Empty else OptRef((this.get, that.get))

  /**
   * Apply side effect only if OptRef is empty. It's a bit like foreach for OptRef.Empty
   *
   * @param sideEffect - code to be executed if optRef is empty
   * @return the same optRef
   * @example {{{captionOptRef.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
   */
  @inline def forEmpty(sideEffect: => Unit): OptRef[A] = {
    if (isEmpty) {
      sideEffect
    }
    this
  }

  override def toString: String =
    if (isEmpty) "OptRef.Empty" else s"OptRef($value)"
}
