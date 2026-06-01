package com.avsystem.commons.misc

import made.Default

import scala.annotation.publicInBinary

object OptRef {
  def apply[A](value: A | Null): OptRef[A] = new OptRef[A](value)
  def unapply[A](opt: OptRef[A]): OptRef[A] = opt // name-based extractor

  def some[A](value: A | Null): OptRef[A] =
    if (value != null) new OptRef[A](value)
    else throw new NullPointerException

  object Boxed {
    def unapply[A, B](optRef: OptRef[B])(using unboxing: Unboxing[A, B]): Opt[A] =
      if (optRef.isEmpty) Opt.Empty else Opt(unboxing.fun(optRef.get))
  }

  given [A] => Conversion[OptRef[A], Iterable[A]] = _.toList

  final val Empty: OptRef[Nothing] = new OptRef[Nothing](null)
  def empty[A]: OptRef[A] = Empty

  @publicInBinary private[misc] val nullFunc: Any => Null = _ => null

  final class WithFilter[+A] @publicInBinary private[OptRef] (self: OptRef[A], p: A => Boolean) {
    def map[B](f: A => B): OptRef[B] = self.filter(p).`map`(f)
    def flatMap[B](f: A => OptRef[B]): OptRef[B] = self.filter(p).`flatMap`(f)
    def foreach[U](f: A => U): Unit = self.filter(p).foreach(f)
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
  }

  // Cast through Default[AnyRef] to avoid erased bridge clash on value class OptRef.
  private val emptyDefault: Default[AnyRef] = () => OptRef.Empty.asInstanceOf[AnyRef]
  given [A] => Default[OptRef[A]] = emptyDefault.asInstanceOf[Default[OptRef[A]]]
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
  * and `null` will be used to represent absence of value. <p/> On Scala 3, nullability is encoded directly in the
  * stored value's type as `A | Null` rather than constraining the type parameter `A` itself — `A` may be any type, and
  * the wrapper carries an explicit `A | Null` slot whose `null` inhabitant represents absence. Empty value is
  * represented internally using `null`, which unfortunately makes [[OptRef]] suffer from SI-7396 (`hashCode` fails on
  * `OptRef.Empty` which means that you can't add [[OptRef]] values into hash sets or use them as hash map keys).
  */
final class OptRef[+A] @publicInBinary private[misc] (private[misc] val value: A | Null)
  extends AnyVal with OptBase[A] with Serializable {
  def isEmpty: Boolean = value == null
  def isDefined: Boolean = !isEmpty
  def nonEmpty: Boolean = isDefined

  def get: A =
    if (isEmpty) throw new NoSuchElementException("empty OptRef") else value.nn

  inline def toOpt: Opt[A] =
    Opt(value)

  inline def toOption: Option[A] =
    Option(value)

  inline def toNOpt: NOpt[A] =
    if (isEmpty) NOpt.Empty else NOpt(value)

  inline def toOptArg: OptArg[A] =
    if (isEmpty) OptArg.Empty else OptArg(value)

  inline def getOrElse[B >: A](inline default: => B): B =
    if (isEmpty) default else value.nn

  inline def orNull[B >: A](using ev: Null <:< B): B =
    value.asInstanceOf[B]

  inline def map[B](inline f: A => B | Null): OptRef[B] =
    if (isEmpty) OptRef.Empty else OptRef(f(value.nn))

  inline def fold[B](inline ifEmpty: => B)(inline f: A => B): B =
    if (isEmpty) ifEmpty else f(value.nn)

  /** The same as [[fold]] but takes arguments in a single parameter list for better type inference.
    */
  inline def mapOr[B](inline ifEmpty: => B, inline f: A => B): B =
    if (isEmpty) ifEmpty else f(value.nn)

  inline def flatMap[B](inline f: A => OptRef[B]): OptRef[B] =
    if (isEmpty) OptRef.Empty else f(value.nn)

  inline def filter(inline p: A => Boolean): OptRef[A] =
    if (isEmpty || p(value.nn)) this else OptRef.Empty

  inline def withFilter(inline p: A => Boolean): OptRef.WithFilter[A] =
    new OptRef.WithFilter[A](this, p)

  inline def filterNot(inline p: A => Boolean): OptRef[A] =
    if (isEmpty || !p(value.nn)) this else OptRef.Empty

  inline def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && value.nn == elem

  inline def exists(inline p: A => Boolean): Boolean =
    !isEmpty && p(value.nn)

  inline def forall(inline p: A => Boolean): Boolean =
    isEmpty || p(value.nn)

  inline def foreach[U](inline f: A => U): Unit = {
    if (!isEmpty) f(value.nn)
  }

  inline def collect[B](inline pf: PartialFunction[A, B]): OptRef[B] =
    if (!isEmpty) new OptRef(pf.applyOrElse(value.nn, OptRef.nullFunc)) else OptRef.Empty

  inline def orElse[B >: A](inline alternative: => OptRef[B]): OptRef[B] =
    if (isEmpty) alternative else this

  inline def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(value.nn)

  inline def toList: List[A] =
    if (isEmpty) List() else new ::(value.nn, Nil)

  inline def toRight[X](inline left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(value.nn)

  inline def toLeft[X](inline right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(value.nn)

  inline def zip[B](that: OptRef[B]): OptRef[(A, B)] =
    if (isEmpty || that.isEmpty) OptRef.Empty else OptRef((this.get, that.get))

  /** Apply side effect only if OptRef is empty. It's a bit like foreach for OptRef.Empty
    *
    * @param sideEffect - code to be executed if optRef is empty
    * @return the same optRef
    * @example {{{captionOptRef.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
    */
  inline def forEmpty(inline sideEffect: => Unit): OptRef[A] = {
    if (isEmpty) {
      sideEffect
    }
    this
  }

  override def toString: String =
    if (isEmpty) "OptRef.Empty" else s"OptRef($value)"
}
