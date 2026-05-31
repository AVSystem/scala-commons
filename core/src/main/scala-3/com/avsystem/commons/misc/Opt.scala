package com.avsystem.commons.misc

import made.Default

import scala.annotation.publicInBinary

object Opt {
  // Used as Opt's raw value to represent empty Opt. Unfortunately, null can't be used for that purpose
  // because https://github.com/scala/bug/issues/7396
  @publicInBinary private[misc] object EmptyMarker extends Serializable

  def apply[A](value: A | Null): Opt[A] = if (value != null) new Opt[A](value) else Opt.Empty
  def unapply[A](opt: Opt[A]): Opt[A] = opt // name-based extractor

  def some[A](value: A): Opt[A] =
    if (value != null) new Opt[A](value)
    else throw new NullPointerException

  given [A] => Conversion[Opt[A], Iterable[A]] = _.toList

  final val Empty: Opt[Nothing] = new Opt(EmptyMarker)

  def empty[A]: Opt[A] = Empty

  @publicInBinary private[misc] val emptyMarkerFunc: Any => Any = _ => EmptyMarker

  final class WithFilter[+A] @publicInBinary private[Opt] (self: Opt[A], p: A => Boolean) {
    def map[B](f: A => B): Opt[B] = self.filter(p).`map`(f)
    def flatMap[B](f: A => Opt[B]): Opt[B] = self.filter(p).`flatMap`(f)
    def foreach[U](f: A => U): Unit = self.filter(p).foreach(f)
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
  }

  extension [A](opt: => Opt[A]) {

    /**
     * When a given condition is true, evaluates the `opt` argument and returns it. When the condition is false, `opt`
     * is not evaluated and [[Opt.Empty]] is returned.
     */
    inline def when(inline cond: Boolean): Opt[A] = if (cond) opt else Opt.Empty

    /**
     * Unless a given condition is true, this will evaluate the `opt` argument and return it. Otherwise, `opt` is not
     * evaluated and [[Opt.Empty]] is returned.
     */
    inline def unless(inline cond: Boolean): Opt[A] = when(!cond)
  }

  // Cast through Default[AnyRef] to avoid erased bridge clash on value class Opt.
  private val emptyDefault: Default[AnyRef] = () => Opt.Empty.asInstanceOf[AnyRef]
  given [A] => Default[Opt[A]] = emptyDefault.asInstanceOf[Default[Opt[A]]]
}

/**
 * Like [[Option]] but implemented as value class (avoids boxing) and treats `null` as no value. Therefore, there is no
 * equivalent for `Some(null)`.
 *
 * If you need a value-class version of [[Option]] which differentiates between no value and `null` value, use
 * [[NOpt]].
 *
 * WARNING: Unfortunately, using `Opt` in pattern matches turns off the exhaustivity checking. Please be aware of that
 * tradeoff.
 */
final class Opt[+A] @publicInBinary private[misc] (@publicInBinary private[misc] val rawValue: Any)
  extends AnyVal with OptBase[A] with Serializable {

  import Opt.*

  private def value: A = rawValue.asInstanceOf[A]

  def isEmpty: Boolean = rawValue.asInstanceOf[AnyRef] eq EmptyMarker
  def isDefined: Boolean = !isEmpty
  def nonEmpty: Boolean = isDefined

  def get: A =
    if (isEmpty) throw new NoSuchElementException("empty Opt") else value

  inline def boxed[B](using boxing: Boxing[A, B]): Opt[B] =
    map(boxing.fun)

  inline def boxedOrNull[B](using boxing: Boxing[A, B]): B | Null =
    if (isEmpty) null else boxing.fun(value)

  inline def unboxed[B](using unboxing: Unboxing[B, A]): Opt[B] =
    map(unboxing.fun)

  inline def toOption: Option[A] =
    if (isEmpty) None else Some(value)

  inline def toOptRef[B](using boxing: Boxing[A, B]): OptRef[B] =
    if (isEmpty) OptRef.Empty else OptRef(boxing.fun(value))

  inline def toNOpt: NOpt[A] =
    if (isEmpty) NOpt.Empty else NOpt(value)

  inline def toOptArg: OptArg[A] =
    if (isEmpty) OptArg.Empty else OptArg(value)

  inline def getOrElse[B >: A](inline default: => B): B =
    if (isEmpty) default else value

  inline def orNull[B >: A](using ev: Null <:< B): B =
    if (isEmpty) ev(null) else value

  /**
   * Analogous to `Option.map` except that when mapping function returns `null`, empty `Opt` is returned as a result.
   */
  inline def map[B](inline f: A => B): Opt[B] =
    if (isEmpty) Opt.Empty else Opt(f(value))

  inline def fold[B](inline ifEmpty: => B)(inline f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  /**
   * The same as [[fold]] but takes arguments in a single parameter list for better type inference.
   */
  inline def mapOr[B](inline ifEmpty: => B, inline f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  inline def flatMap[B](inline f: A => Opt[B]): Opt[B] =
    if (isEmpty) Opt.Empty else f(value)

  inline def flatten[B](using ev: A <:< Opt[B]): Opt[B] =
    if (isEmpty) Opt.Empty else ev(value)

  inline def filter(inline p: A => Boolean): Opt[A] =
    if (isEmpty || p(value)) this else Opt.Empty

  inline def withFilter(inline p: A => Boolean): Opt.WithFilter[A] =
    new Opt.WithFilter[A](this, p)

  inline def filterNot(inline p: A => Boolean): Opt[A] =
    if (isEmpty || !p(value)) this else Opt.Empty

  inline def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && value == elem

  inline def exists(inline p: A => Boolean): Boolean =
    !isEmpty && p(value)

  inline def forall(inline p: A => Boolean): Boolean =
    isEmpty || p(value)

  inline def foreach[U](inline f: A => U): Unit = {
    if (!isEmpty) f(value)
  }

  /**
   * Analogous to `Option.collect` except that when the function returns `null`, empty `Opt` is returned as a result.
   */
  inline def collect[B](inline pf: PartialFunction[A, B]): Opt[B] =
    if (!isEmpty) {
      val res = pf.applyOrElse(value, Opt.emptyMarkerFunc)
      new Opt(if (res == null) EmptyMarker else res)
    } else Opt.Empty

  inline def orElse[B >: A](inline alternative: => Opt[B]): Opt[B] =
    if (isEmpty) alternative else this

  inline def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(value)

  inline def toList: List[A] =
    if (isEmpty) List() else value :: Nil

  inline def toRight[X](inline left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(value)

  inline def toLeft[X](inline right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(value)

  inline def zip[B](that: Opt[B]): Opt[(A, B)] =
    if (isEmpty || that.isEmpty) Opt.Empty else Opt((this.get, that.get))

  /**
   * Apply side effect only if Opt is empty. It's a bit like foreach for Opt.Empty
   *
   * @param sideEffect - code to be executed if opt is empty
   * @return the same opt
   * @example {{{captionOpt.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
   */
  inline def forEmpty(inline sideEffect: => Unit): Opt[A] = {
    if (isEmpty) {
      sideEffect
    }
    this
  }

  override def toString: String =
    if (isEmpty) "Opt.Empty" else s"Opt($value)"

}
