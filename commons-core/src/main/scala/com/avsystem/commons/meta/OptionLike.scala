package com.avsystem.commons
package meta

sealed trait OptionLike[O] {
  type Value
  def none: O
  def some(value: Value): O
  def isDefined(opt: O): Boolean
  def get(opt: O): Value

  def fold[B](opt: O, ifEmpty: => B)(f: Value => B): B = if (isDefined(opt)) f(get(opt)) else ifEmpty
  def getOrElse[B >: Value](opt: O, default: => B): B = if (isDefined(opt)) get(opt) else default
}

final class OptionLikeImpl[O, A](
  empty: O,
  someFun: A => O,
  isDefinedFun: O => Boolean,
  getFun: O => A,
) extends OptionLike[O] {
  type Value = A
  def none: O = empty
  def some(value: A): O = someFun(value)
  def isDefined(opt: O): Boolean = isDefinedFun(opt)
  def get(opt: O): A = getFun(opt)
}
object OptionLike {
  type Aux[O, V] = OptionLike[O] {type Value = V}

  implicit def optionOptionLike[A]: OptionLikeImpl[Option[A], A] =
    new OptionLikeImpl(None, Some(_), _.isDefined, _.get)

  implicit def optOptionLike[A]: OptionLikeImpl[Opt[A], A] =
    new OptionLikeImpl(Opt.Empty, Opt(_), _.isDefined, _.get)

  implicit def optRefOptionLike[A >: Null]: OptionLikeImpl[OptRef[A], A] =
    new OptionLikeImpl(OptRef.Empty, OptRef(_), _.isDefined, _.get)

  implicit def optArgOptionLike[A]: OptionLikeImpl[OptArg[A], A] =
    new OptionLikeImpl(OptArg.Empty, OptArg(_), _.isDefined, _.get)

  implicit def nOptOptionLike[A]: OptionLikeImpl[NOpt[A], A] =
    new OptionLikeImpl(NOpt.Empty, NOpt(_), _.isDefined, _.get)
}
