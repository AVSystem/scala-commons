package com.avsystem.commons.misc

import com.avsystem.commons.IIterable

import scala.annotation.publicInBinary

object OptRef {
  def apply[A >: Null](value: A): OptRef[A] = new OptRef[A](value)
  def unapply[A >: Null](opt: OptRef[A]): OptRef[A] = opt // name-based extractor

  def some[A >: Null](value: A): OptRef[A] =
    if (value != null) new OptRef[A](value)
    else throw new NullPointerException

  object Boxed {
    def unapply[A, B >: Null](optRef: OptRef[B])(implicit unboxing: Unboxing[A, B]): Opt[A] =
      if (optRef.isEmpty) Opt.Empty else Opt(unboxing.fun(optRef.get))
  }

  implicit def opt2Iterable[A >: Null](xo: OptRef[A]): IIterable[A] = xo.toList

  final val Empty: OptRef[Null] = new OptRef[Null](null)

  def empty[A >: Null]: OptRef[A] = Empty

  @publicInBinary private[misc] val nullFunc: Any => Null = _ => null

  final class WithFilter[+A >: Null] @publicInBinary private[OptRef] (self: OptRef[A], p: A => Boolean) {
    def map[B >: Null](f: A => B): OptRef[B] = self.filter(p) map f
    def flatMap[B >: Null](f: A => OptRef[B]): OptRef[B] = self.filter(p) flatMap f
    def foreach[U](f: A => U): Unit = self.filter(p).foreach(f)
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
  }
}

/** Like [[Opt]] but has better Java interop thanks to the fact that wrapped value has type `A` instead of `Any`. For
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
final class OptRef[+A >: Null] @publicInBinary private[misc] (@publicInBinary private[misc] val value: A)
  extends AnyVal with OptBase[A] with Serializable {
  def isEmpty: Boolean = value == null
  def isDefined: Boolean = !isEmpty
  def nonEmpty: Boolean = isDefined

  def get: A =
    if (isEmpty) throw new NoSuchElementException("empty OptRef") else value

  def toOpt: Opt[A] =
    Opt(value)

  def toOption: Option[A] =
    Option(value)

  def toNOpt: NOpt[A] =
    if (isEmpty) NOpt.Empty else NOpt(value)

  def toOptArg: OptArg[A] =
    if (isEmpty) OptArg.Empty else OptArg(value)

  inline def getOrElse[B >: A](inline default: B): B =
    if (isEmpty) default else value

  def orNull[B >: A](implicit ev: Null <:< B): B =
    value.asInstanceOf[B]

  inline def map[B >: Null](inline f: A => B): OptRef[B] =
    if (isEmpty) OptRef.Empty else OptRef(f(value))

  inline def fold[B](inline ifEmpty: B)(inline f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  /** The same as [[fold]] but takes arguments in a single parameter list for better type inference.
    */
  inline def mapOr[B](inline ifEmpty: B, inline f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  inline def flatMap[B >: Null](inline f: A => OptRef[B]): OptRef[B] =
    if (isEmpty) OptRef.Empty else f(value)

  inline def filter(inline p: A => Boolean): OptRef[A] =
    if (isEmpty || p(value)) this else OptRef.Empty

  inline def withFilter(inline p: A => Boolean): OptRef.WithFilter[A] =
    new OptRef.WithFilter[A](this, p)

  inline def filterNot(inline p: A => Boolean): OptRef[A] =
    if (isEmpty || !p(value)) this else OptRef.Empty

  def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && value == elem

  inline def exists(inline p: A => Boolean): Boolean =
    !isEmpty && p(value)

  inline def forall(inline p: A => Boolean): Boolean =
    isEmpty || p(value)

  inline def foreach[U](inline f: A => U): Unit = {
    if (!isEmpty) f(value)
  }

  inline def collect[B >: Null](inline pf: PartialFunction[A, B]): OptRef[B] =
    if (!isEmpty) new OptRef(pf.applyOrElse(value, OptRef.nullFunc)) else OptRef.Empty

  inline def orElse[B >: A](inline alternative: OptRef[B]): OptRef[B] =
    if (isEmpty) alternative else this

  def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(value)

  def toList: List[A] =
    if (isEmpty) List() else new ::(value, Nil)

  inline def toRight[X](inline left: X): Either[X, A] =
    if (isEmpty) Left(left) else Right(value)

  inline def toLeft[X](inline right: X): Either[A, X] =
    if (isEmpty) Right(right) else Left(value)

  def zip[B >: Null](that: OptRef[B]): OptRef[(A, B)] =
    if (isEmpty || that.isEmpty) OptRef.Empty else OptRef((this.get, that.get))

  /** Apply side effect only if OptRef is empty. It's a bit like foreach for OptRef.Empty
    *
    * @param sideEffect - code to be executed if optRef is empty
    * @return the same optRef
    * @example {{{captionOptRef.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
    */
  inline def forEmpty(inline sideEffect: Unit): OptRef[A] = {
    if (isEmpty) {
      sideEffect
    }
    this
  }

  override def toString: String =
    if (isEmpty) "OptRef.Empty" else s"OptRef($value)"
}
