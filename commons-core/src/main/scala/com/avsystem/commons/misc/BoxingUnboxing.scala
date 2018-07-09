package com.avsystem.commons
package misc

case class Boxing[-A, +B](fun: A => B) extends AnyVal
object Boxing extends LowPrioBoxing {
  def fromImplicitConv[A, B](implicit conv: A => B): Boxing[A, B] = Boxing(conv)

  implicit val BooleanBoxing: Boxing[Boolean, JBoolean] = fromImplicitConv
  implicit val ByteBoxing: Boxing[Byte, JByte] = fromImplicitConv
  implicit val ShortBoxing: Boxing[Short, JShort] = fromImplicitConv
  implicit val IntBoxing: Boxing[Int, JInteger] = fromImplicitConv
  implicit val LongBoxing: Boxing[Long, JLong] = fromImplicitConv
  implicit val FloatBoxing: Boxing[Float, JFloat] = fromImplicitConv
  implicit val DoubleBoxing: Boxing[Double, JDouble] = fromImplicitConv
}
trait LowPrioBoxing { this: Boxing.type =>
  implicit def nullableBoxing[A >: Null]: Boxing[A, A] = Boxing(identity)
}

case class Unboxing[+A, -B](fun: B => A) extends AnyVal
object Unboxing extends LowPrioUnboxing {
  def fromImplicitConv[A, B](implicit conv: B => A): Unboxing[A, B] = Unboxing(conv)

  implicit val BooleanUnboxing: Unboxing[Boolean, JBoolean] = fromImplicitConv
  implicit val ByteUnboxing: Unboxing[Byte, JByte] = fromImplicitConv
  implicit val ShortUnboxing: Unboxing[Short, JShort] = fromImplicitConv
  implicit val IntUnboxing: Unboxing[Int, JInteger] = fromImplicitConv
  implicit val LongUnboxing: Unboxing[Long, JLong] = fromImplicitConv
  implicit val FloatUnboxing: Unboxing[Float, JFloat] = fromImplicitConv
  implicit val DoubleUnboxing: Unboxing[Double, JDouble] = fromImplicitConv
}
trait LowPrioUnboxing { this: Unboxing.type =>
  implicit def nullableUnboxing[A >: Null]: Unboxing[A, A] = Unboxing(identity)
}
