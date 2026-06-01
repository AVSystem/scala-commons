package com.avsystem.commons
package misc

case class Boxing[-A, +B](fun: A => B) extends AnyVal
object Boxing extends LowPrioBoxing {
  def fromImplicitConv[A, B](using conv: A => B): Boxing[A, B] = Boxing(conv)

  given Boxing[Boolean, JBoolean] = fromImplicitConv
  given Boxing[Byte, JByte] = fromImplicitConv
  given Boxing[Short, JShort] = fromImplicitConv
  given Boxing[Int, JInteger] = fromImplicitConv
  given Boxing[Long, JLong] = fromImplicitConv
  given Boxing[Float, JFloat] = fromImplicitConv
  given Boxing[Double, JDouble] = fromImplicitConv

  // Source-compat aliases for callers that previously referenced these by name.
  @deprecated("Use summon[Boxing[Boolean, JBoolean]]", since = "scala-3-port")
  def BooleanBoxing: Boxing[Boolean, JBoolean] = summon
  @deprecated("Use summon[Boxing[Byte, JByte]]", since = "scala-3-port")
  def ByteBoxing: Boxing[Byte, JByte] = summon
  @deprecated("Use summon[Boxing[Short, JShort]]", since = "scala-3-port")
  def ShortBoxing: Boxing[Short, JShort] = summon
  @deprecated("Use summon[Boxing[Int, JInteger]]", since = "scala-3-port")
  def IntBoxing: Boxing[Int, JInteger] = summon
  @deprecated("Use summon[Boxing[Long, JLong]]", since = "scala-3-port")
  def LongBoxing: Boxing[Long, JLong] = summon
  @deprecated("Use summon[Boxing[Float, JFloat]]", since = "scala-3-port")
  def FloatBoxing: Boxing[Float, JFloat] = summon
  @deprecated("Use summon[Boxing[Double, JDouble]]", since = "scala-3-port")
  def DoubleBoxing: Boxing[Double, JDouble] = summon
}
trait LowPrioBoxing { this: Boxing.type =>
  given nullableBoxing[A >: Null]: Boxing[A, A] = Boxing(identity)
}

case class Unboxing[+A, -B](fun: B => A) extends AnyVal
object Unboxing extends LowPrioUnboxing {
  def fromImplicitConv[A, B](using conv: B => A): Unboxing[A, B] = Unboxing(conv)

  given Unboxing[Boolean, JBoolean] = fromImplicitConv
  given Unboxing[Byte, JByte] = fromImplicitConv
  given Unboxing[Short, JShort] = fromImplicitConv
  given Unboxing[Int, JInteger] = fromImplicitConv
  given Unboxing[Long, JLong] = fromImplicitConv
  given Unboxing[Float, JFloat] = fromImplicitConv
  given Unboxing[Double, JDouble] = fromImplicitConv

  // Source-compat aliases for callers that previously referenced these by name.
  @deprecated("Use summon[Unboxing[Boolean, JBoolean]]", since = "scala-3-port")
  def BooleanUnboxing: Unboxing[Boolean, JBoolean] = summon
  @deprecated("Use summon[Unboxing[Byte, JByte]]", since = "scala-3-port")
  def ByteUnboxing: Unboxing[Byte, JByte] = summon
  @deprecated("Use summon[Unboxing[Short, JShort]]", since = "scala-3-port")
  def ShortUnboxing: Unboxing[Short, JShort] = summon
  @deprecated("Use summon[Unboxing[Int, JInteger]]", since = "scala-3-port")
  def IntUnboxing: Unboxing[Int, JInteger] = summon
  @deprecated("Use summon[Unboxing[Long, JLong]]", since = "scala-3-port")
  def LongUnboxing: Unboxing[Long, JLong] = summon
  @deprecated("Use summon[Unboxing[Float, JFloat]]", since = "scala-3-port")
  def FloatUnboxing: Unboxing[Float, JFloat] = summon
  @deprecated("Use summon[Unboxing[Double, JDouble]]", since = "scala-3-port")
  def DoubleUnboxing: Unboxing[Double, JDouble] = summon
}
trait LowPrioUnboxing { this: Unboxing.type =>
  given nullableUnboxing[A >: Null]: Unboxing[A, A] = Unboxing(identity)
}
