package com.avsystem.commons.misc

import made.Default

import scala.annotation.{publicInBinary, targetName}
import scala.language.implicitConversions

object OptArg {

  /**
   * This conversion allows you to pass unwrapped values where `OptArg` is required. Kept as `implicit def` (not a
   * `given Conversion`) because a polymorphic `Conversion[A, OptArg[A]]` generates a clashing bridge: both `A` and the
   * `OptArg` value class erase to `Object`.
   */
  implicit def argToOptArg[A](value: A): OptArg[A] = OptArg(value)

  // additional implicits to cover most common, safe numeric promotions
  @targetName("intToOptArgLong")
  given Conversion[Int, OptArg[Long]] = OptArg(_)
  @targetName("intToOptArgDouble")
  given Conversion[Int, OptArg[Double]] = OptArg(_)

  @publicInBinary private[misc] object EmptyMarker extends Serializable

  def apply[A](value: A | Null): OptArg[A] = new OptArg[A](if (value != null) value else EmptyMarker)
  def unapply[A](opt: OptArg[A]): OptArg[A] = opt // name-based extractor

  def some[A](value: A): OptArg[A] =
    if (value != null) new OptArg[A](value)
    else throw new NullPointerException

  val Empty: OptArg[Nothing] = new OptArg(EmptyMarker)
  def empty[A]: OptArg[A] = Empty

  // Cast through Default[AnyRef] to avoid erased bridge clash on value class OptArg.
  private val emptyDefault: Default[AnyRef] = () => OptArg.Empty.asInstanceOf[AnyRef]
  given [A] => Default[OptArg[A]] = emptyDefault.asInstanceOf[Default[OptArg[A]]]
}

/**
 * [[OptArg]] is like [[Opt]] except it's intended to be used to type-safely express optional method/constructor
 * parameters while at the same time avoiding having to explicitly wrap arguments when passing them (thanks to the
 * implicit conversion from `A` to `OptArg[A]`). For example:
 *
 * {{{
 *   def takesMaybeString(str: OptArg[String] = OptArg.Empty) = ???
 *
 *   takesMaybeString()         // default empty value is used
 *   takesMaybeString("string") // no explicit wrapping into OptArg required
 * }}}
 *
 * Note that like [[Opt]], [[OptArg]] assumes its underlying value to be non-null and `null` is translated into
 * `OptArg.Empty`. <br/> It is strongly recommended that [[OptArg]] type is used ONLY in signatures where implicit
 * conversion `A => OptArg[A]` is intended to work. You should not use [[OptArg]] as a general-purpose "optional value"
 * type - other types like [[Opt]], [[NOpt]] and `Option` serve that purpose. For this reason [[OptArg]] deliberately
 * does not have any "transforming" methods like `map`, `flatMap`, `orElse`, etc. Instead it's recommended that
 * [[OptArg]] is converted to [[Opt]], [[NOpt]] or `Option` as soon as possible (using `toOpt`, `toNOpt` and `toOption`
 * methods).
 */
final class OptArg[+A] @publicInBinary private[misc] (@publicInBinary private[misc] val rawValue: Any)
  extends AnyVal with OptBase[A] with Serializable {
  import OptArg.*

  private def value: A = rawValue.asInstanceOf[A]

  def get: A = if (isEmpty) throw new NoSuchElementException("empty OptArg") else value

  def isEmpty: Boolean = rawValue.asInstanceOf[AnyRef] eq EmptyMarker
  def isDefined: Boolean = !isEmpty
  def nonEmpty: Boolean = isDefined

  inline def boxedOrNull[B](using boxing: Boxing[A, B]): B | Null =
    if (isEmpty) null else boxing.fun(value)

  inline def toOpt: Opt[A] =
    if (isEmpty) Opt.Empty else Opt(value)

  inline def toOption: Option[A] =
    if (isEmpty) None else Some(value)

  inline def toNOpt: NOpt[A] =
    if (isEmpty) NOpt.Empty else NOpt.some(value)

  inline def toOptRef[B](using boxing: Boxing[A, B]): OptRef[B] =
    if (isEmpty) OptRef.Empty else OptRef(boxing.fun(value))

  inline def getOrElse[B >: A](inline default: => B): B =
    if (isEmpty) default else value

  inline def orNull[B >: A](using ev: Null <:< B): B =
    if (isEmpty) ev(null) else value

  inline def fold[B](inline ifEmpty: => B)(inline f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  /**
   * The same as [[fold]] but takes arguments in a single parameter list for better type inference.
   */
  inline def mapOr[B](inline ifEmpty: => B, inline f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  inline def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && value == elem

  inline def exists(inline p: A => Boolean): Boolean =
    !isEmpty && p(value)

  inline def forall(inline p: A => Boolean): Boolean =
    isEmpty || p(value)

  inline def foreach[U](inline f: A => U): Unit = {
    if (!isEmpty) f(value)
  }

  inline def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(value)

  inline def toList: List[A] =
    if (isEmpty) List() else new ::(value, Nil)

  inline def toRight[X](inline left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(value)

  inline def toLeft[X](inline right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(value)

  /**
   * Apply side effect only if OptArg is empty. It's a bit like foreach for OptArg.Empty
   *
   * @param sideEffect - code to be executed if optArg is empty
   * @return the same optArg
   * @example {{{captionOptArg.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
   */
  inline def forEmpty(inline sideEffect: => Unit): OptArg[A] = {
    if (isEmpty) {
      sideEffect
    }
    this
  }

  override def toString: String =
    if (isEmpty) "OptArg.Empty" else s"OptArg($value)"
}
