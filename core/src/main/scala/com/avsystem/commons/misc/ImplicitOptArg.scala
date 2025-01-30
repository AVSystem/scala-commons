package com.avsystem.commons.misc

object ImplicitOptArg {
  /**
    * This implicit conversion allows you to pass unwrapped values where `OptArg` is required.
    */
  implicit def argToImplicitOptArg[A](value: A): ImplicitOptArg[A] = ImplicitOptArg(value)
  implicit def implicitArgToImplicitOptArg[A](implicit value: A): ImplicitOptArg[A] = ImplicitOptArg(value)

  // additional implicits to cover most common, safe numeric promotions
  implicit def intToOptArgLong(int: Int): ImplicitOptArg[Long] = ImplicitOptArg(int)
  implicit def intToOptArgDouble(int: Int): ImplicitOptArg[Double] = ImplicitOptArg(int)

  private object EmptyMarker extends Serializable

  def apply[A](value: A): ImplicitOptArg[A] = new ImplicitOptArg[A](if (value != null) value else EmptyMarker)
  def unapply[A](opt: ImplicitOptArg[A]): ImplicitOptArg[A] = opt //name-based extractor

  def some[A](value: A): ImplicitOptArg[A] =
    if (value != null) new ImplicitOptArg[A](value)
    else throw new NullPointerException

  val Empty: ImplicitOptArg[Nothing] = new ImplicitOptArg(EmptyMarker)
  def empty[A]: ImplicitOptArg[A] = Empty
}

/**
  * [[ImplicitOptArg]] is like [[OptArg]] except it's intended to be used to type-safely express optional implicit method/constructor
  * parameters while at the same time avoiding having to explicitly wrap arguments when passing them
  * (thanks to the implicit conversion from `implicit A` to `ImplicitOptArg[A]`). For example:
  *
  * {{{
  *   def takesMaybeString(implicit str: ImplicitOptArg[String]) = ???
  *
  *   implicit val str: String = "string"
  *   takesMaybeString()                            // str is used
  *   takesMaybeString(using str)                   // str is used explicitly
  *   takesMaybeString(using ImplicitOptArg.Empty)  // Empty is used explicitly
  * }}}
  *
  * Note that like [[Opt]], [[ImplicitOptArg]] assumes its underlying value to be non-null and `null` is translated into `ImplicitOptArg.Empty`.
  * <br/>
  * It is strongly recommended that [[ImplicitOptArg]] type is used without default argument.
  * You should not use [[ImplicitOptArg]] as a general-purpose "optional value" type - other types like
  * [[Opt]], [[NOpt]] and `Option` serve that purpose. For this reason [[ImplicitOptArg]] deliberately does not have any "transforming"
  * methods like `map`, `flatMap`, `orElse`, etc. Instead it's recommended that [[ImplicitOptArg]] is converted to [[Opt]],
  * [[NOpt]] or `Option` as soon as possible (using `toOpt`, `toNOpt` and `toOption` methods).
  */
final class ImplicitOptArg[+A] private(private val rawValue: Any) extends AnyVal with OptBase[A] with Serializable {

  import ImplicitOptArg.*

  private def value: A = rawValue.asInstanceOf[A]

  @inline def get: A = if (isEmpty) throw new NoSuchElementException("empty ImplicitOptArg") else value

  @inline def isEmpty: Boolean = rawValue.asInstanceOf[AnyRef] eq EmptyMarker
  @inline def isDefined: Boolean = !isEmpty
  @inline def nonEmpty: Boolean = isDefined

  @inline def boxedOrNull[B >: Null](implicit boxing: Boxing[A, B]): B =
    if (isEmpty) null else boxing.fun(value)

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

  /**
    * The same as [[fold]] but takes arguments in a single parameter list for better type inference.
    */
  @inline def mapOr[B](ifEmpty: => B, f: A => B): B =
    if (isEmpty) ifEmpty else f(value)

  @inline def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && value == elem

  @inline def exists(p: A => Boolean): Boolean =
    !isEmpty && p(value)

  @inline def forall(p: A => Boolean): Boolean =
    isEmpty || p(value)

  @inline def foreach[U](f: A => U): Unit = {
    if (!isEmpty) f(value)
  }

  @inline def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty else Iterator.single(value)

  @inline def toList: List[A] =
    if (isEmpty) List() else value :: Nil

  @inline def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(value)

  @inline def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(value)

  /**
    * Apply side effect only if ImplicitOptArg is empty. It's a bit like foreach for ImplicitOptArg.Empty
    *
    * @param sideEffect - code to be executed if optArg is empty
    * @return the same ImplicitOptArg
    * @example {{{captionOptArg.forEmpty(logger.warn("caption is empty")).foreach(setCaption)}}}
    */
  @inline def forEmpty(sideEffect: => Unit): ImplicitOptArg[A] = {
    if (isEmpty) {
      sideEffect
    }
    this
  }

  override def toString: String =
    if (isEmpty) "ImplicitOptArg.Empty" else s"ImplicitOptArg($value)"
}
