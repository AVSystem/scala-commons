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
}
trait LowPrioBoxing { this: Boxing.type =>
  given [A] => Boxing[A, A] = Boxing(identity)
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
}
trait LowPrioUnboxing { this: Unboxing.type =>
  given [A] => Unboxing[A, A] = Unboxing(identity)
}
