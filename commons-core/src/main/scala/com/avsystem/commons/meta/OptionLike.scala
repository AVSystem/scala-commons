package com.avsystem.commons
package meta

sealed trait OptionLike[O] {
  type Value
  def none: O
  def some(value: Value): O
  def fold[B](opt: O, ifEmpty: => B)(f: Value => B): B
  def getOrElse[B >: Value](opt: O, default: => B): B = fold(opt, default)(v => v)
}
trait BaseOptionLike[O, A] extends OptionLike[O] {
  type Value = A
}
object OptionLike {
  type Aux[O, V] = OptionLike[O] {type Value = V}

  implicit def optionOptionLike[A]: BaseOptionLike[Option[A], A] = new BaseOptionLike[Option[A], A] {
    def none: Option[A] = None
    def some(value: A): Option[A] = Some(value)
    def fold[B](opt: Option[A], ifEmpty: => B)(f: A => B): B = opt.fold(ifEmpty)(f)
    override def getOrElse[B >: A](opt: Option[A], default: => B): B = opt.getOrElse(default)
  }

  implicit def optOptionLike[A]: BaseOptionLike[Opt[A], A] = new BaseOptionLike[Opt[A], A] {
    def some(value: A): Opt[A] = Opt.some(value)
    def none: Opt[A] = Opt.empty
    def fold[B](opt: Opt[A], ifEmpty: => B)(f: A => B): B = opt.fold(ifEmpty)(f)
    override def getOrElse[B >: A](opt: Opt[A], default: => B): B = opt.getOrElse(default)
  }

  implicit def optRefOptionLike[A >: Null]: BaseOptionLike[OptRef[A], A] = new BaseOptionLike[OptRef[A], A] {
    def none: OptRef[A] = OptRef.empty
    def some(value: A): OptRef[A] = OptRef.some(value)
    def fold[B](opt: OptRef[A], ifEmpty: => B)(f: A => B): B = opt.fold(ifEmpty)(f)
    override def getOrElse[B >: A](opt: OptRef[A], default: => B): B = opt.getOrElse(default)
  }

  implicit def optArgOptionLike[A]: BaseOptionLike[OptArg[A], A] = new BaseOptionLike[OptArg[A], A] {
    def none: OptArg[A] = OptArg.empty
    def some(value: A): OptArg[A] = OptArg.some(value)
    def fold[B](opt: OptArg[A], ifEmpty: => B)(f: A => B): B = opt.fold(ifEmpty)(f)
    override def getOrElse[B >: A](opt: OptArg[A], default: => B): B = opt.getOrElse(default)
  }

  implicit def nOptOptionLike[A]: BaseOptionLike[NOpt[A], A] = new BaseOptionLike[NOpt[A], A] {
    def none: NOpt[A] = NOpt.empty
    def some(value: A): NOpt[A] = NOpt.some(value)
    def fold[B](opt: NOpt[A], ifEmpty: => B)(f: A => B): B = opt.fold(ifEmpty)(f)
    override def getOrElse[B >: A](opt: NOpt[A], default: => B): B = opt.getOrElse(default)
  }
}
