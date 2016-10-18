package com.avsystem.commons
package misc

import com.avsystem.commons.misc.OptArg.EmptyMarker

import scala.language.implicitConversions

object OptArg {
  /**
    * This implicit conversion allows you to pass unwrapped values where `OptArg` is required.
    */
  implicit def argToOptArg[A](value: A): OptArg[A] = OptArg(value)

  private object EmptyMarker extends Serializable

  def apply[A](value: A): OptArg[A] = new OptArg[A](if (value != null) value else EmptyMarker)
  def unapply[A](opt: OptArg[A]): OptArg[A] = opt //name-based extractor

  def some[A](value: A): OptArg[A] =
    if (value != null) new OptArg[A](value)
    else throw new NullPointerException

  val Empty: OptArg[Nothing] = new OptArg(EmptyMarker)
  def empty[A]: OptArg[A] = Empty
}

/**
  * [[OptArg]] is like [[Opt]] except it's intended to be used to type-safely express optional method/constructor
  * parameters while at the same time avoiding having to explicitly wrap arguments when passing them
  * (thanks to the implicit conversion from `A` to `OptArg[A]`). For example:
  *
  * {{{
  *   def takesMaybeString(str: OptArg[String] = OptArg.Empty) = ???
  *
  *   takesMaybeString()         // default empty value is used
  *   takesMaybeString("string") // no explicit wrapping into OptArg required
  * }}}
  *
  * Note that like [[Opt]], [[OptArg]] assumes its underlying value to be non-null and `null` is translated into `OptArg.Empty`.
  */
final class OptArg[+A](private val rawValue: Any) extends AnyVal with Serializable {
  private def value: A = rawValue.asInstanceOf[A]

  @inline def get: A = if (isEmpty) throw new NoSuchElementException("empty OptArg") else value

  @inline def isEmpty: Boolean = rawValue.asInstanceOf[AnyRef] eq EmptyMarker
  @inline def isDefined: Boolean = !isEmpty
  @inline def nonEmpty: Boolean = isDefined

  @inline def toOpt: Opt[A] =
    if (isEmpty) Opt.Empty else Opt(value)

  @inline def toOption: Option[A] =
    if (isEmpty) None else Some(value)

  @inline def toNOpt: NOpt[A] =
    if (isEmpty) NOpt.Empty else NOpt.some(value)

  @inline def toOptRef[B >: Null](implicit boxing: Boxing[A, B]): OptRef[B] =
    if (isEmpty) OptRef.Empty else OptRef(boxing.fun(value))

  @inline def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else value

  @inline def orNull[B >: A](implicit ev: Null <:< B): B =
    if (isEmpty) ev(null) else value

  @inline def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(value)
}
