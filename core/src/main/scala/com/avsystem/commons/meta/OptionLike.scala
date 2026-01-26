package com.avsystem.commons
package meta

import com.avsystem.commons.annotation.bincompat

import java.util as ju

sealed trait OptionLike[O] {
  type Value
  def none: O
  def some(value: Value): O
  def isDefined(opt: O): Boolean
  def get(opt: O): Value

  /**
   * Determines whether `null` values should be collapsed into empty values (i.e. [[none]]). Used primarily by
   * `GenCodec` when deserializing and dealing with e.g. JSON nulls vs missing fields.
   */
  def ignoreNulls: Boolean

  def fold[B](opt: O, ifEmpty: => B)(f: Value => B): B = if (isDefined(opt)) f(get(opt)) else ifEmpty
  def getOrElse[B >: Value](opt: O, default: => B): B = if (isDefined(opt)) get(opt) else default
  def foreach(opt: O, f: Value => Unit): Unit = if (isDefined(opt)) f(get(opt))

  final def convert[OO, V](opt: O, into: OptionLike.Aux[OO, V])(fun: Value => V): OO =
    fold(opt, into.none)(v => into.some(fun(v)))

  final def apply(value: Value): O =
    if (ignoreNulls && (value.asInstanceOf[AnyRef] eq null)) none else some(value)
}

@bincompat
sealed trait BaseOptionLike[O, A] extends OptionLike[O] {
  type Value = A
}

final class OptionLikeImpl[O, A](
  empty: O,
  someFun: A => O,
  isDefinedFun: O => Boolean,
  getFun: O => A,
  val ignoreNulls: Boolean,
) extends BaseOptionLike[O, A] {
  def none: O = empty
  def some(value: A): O = someFun(value)
  def isDefined(opt: O): Boolean = isDefinedFun(opt)
  def get(opt: O): A = getFun(opt)

  @bincompat private[meta] def this(
    empty: O,
    someFun: A => O,
    isDefinedFun: O => Boolean,
    getFun: O => A,
  ) = this(empty, someFun, isDefinedFun, getFun, ignoreNulls = true)
}
object OptionLike {
  implicit def optionOptionLike[A]: BaseOptionLike[Option[A], A] =
    new OptionLikeImpl(None, Some(_), _.isDefined, _.get, ignoreNulls = true)
  implicit def optOptionLike[A]: BaseOptionLike[Opt[A], A] =
    new OptionLikeImpl(Opt.Empty, Opt.some, _.isDefined, _.get, ignoreNulls = true)
  implicit def optRefOptionLike[A]: BaseOptionLike[OptRef[A], A] =
    new OptionLikeImpl(OptRef.Empty, OptRef.some, _.isDefined, _.get, ignoreNulls = true)
  implicit def optArgOptionLike[A]: BaseOptionLike[OptArg[A], A] =
    new OptionLikeImpl(OptArg.Empty, OptArg.some, _.isDefined, _.get, ignoreNulls = true)
  implicit def nOptOptionLike[A]: BaseOptionLike[NOpt[A], A] =
    new OptionLikeImpl(NOpt.Empty, NOpt.some, _.isDefined, _.get, ignoreNulls = false)
  type Aux[O, V] = OptionLike[O] { type Value = V }
}

/**
 * If there is an instance of [[AutoOptionalParam]] for some type `T` then all case class & RPC parameters of type `T`
 * will be treated as if they were annotated with [[com.avsystem.commons.serialization.optionalParam @optionalParam]].
 *
 * As with `@optionalParam` annotation, independently there must be also an instance of [[OptionLike]] for `T` for the
 * entire mechanism to work. See the scaladoc of [[com.avsystem.commons.serialization.optionalParam optionalParam]] for
 * more information.
 */
sealed trait AutoOptionalParam[T]
object AutoOptionalParam {
  def apply[T]: AutoOptionalParam[T] = null.asInstanceOf[AutoOptionalParam[T]]
}

trait AutoOptionalParams {
  implicit def allAutoOptionalParams[T](
    implicit optionLike: OptionLike[T],
  ): AutoOptionalParam[T] = AutoOptionalParam[T]
}
object AutoOptionalParams extends AutoOptionalParams
