package com.avsystem.commons
package meta

import com.avsystem.commons.annotation.bincompat

sealed trait OptionLike[O] {
  type Value
  def none: O
  def some(value: Value): O
  def isDefined(opt: O): Boolean
  def get(opt: O): Value

  def fold[B](opt: O, ifEmpty: => B)(f: Value => B): B = if (isDefined(opt)) f(get(opt)) else ifEmpty
  def getOrElse[B >: Value](opt: O, default: => B): B = if (isDefined(opt)) get(opt) else default
  def foreach(opt: O, f: Value => Unit): Unit = if (isDefined(opt)) f(get(opt))

  final def convert[OO, V](opt: O, into: OptionLike.Aux[OO, V])(fun: Value => V): OO =
    fold(opt, into.none)(v => into.some(fun(v)))
}

@bincompat
sealed trait BaseOptionLike[O, A] extends OptionLike[O] {
  type Value = A
}

final class OptionLikeImpl[O, A](
  empty: O,
  someFun: A => O,
  isDefinedFun: O => Boolean,
  getFun: O => A
) extends BaseOptionLike[O, A] {
  def none: O = empty
  def some(value: A): O = someFun(value)
  def isDefined(opt: O): Boolean = isDefinedFun(opt)
  def get(opt: O): A = getFun(opt)
}
object OptionLike {
  type Aux[O, V] = OptionLike[O] {type Value = V}

  implicit def optionOptionLike[A]: BaseOptionLike[Option[A], A] =
    new OptionLikeImpl(None, Some(_), _.isDefined, _.get)

  implicit def optOptionLike[A]: BaseOptionLike[Opt[A], A] =
    new OptionLikeImpl(Opt.Empty, Opt.some, _.isDefined, _.get)

  implicit def optRefOptionLike[A >: Null]: BaseOptionLike[OptRef[A], A] =
    new OptionLikeImpl(OptRef.Empty, OptRef.some, _.isDefined, _.get)

  implicit def optArgOptionLike[A]: BaseOptionLike[OptArg[A], A] =
    new OptionLikeImpl(OptArg.Empty, OptArg.some, _.isDefined, _.get)

  implicit def nOptOptionLike[A]: BaseOptionLike[NOpt[A], A] =
    new OptionLikeImpl(NOpt.Empty, NOpt.some, _.isDefined, _.get)
}
