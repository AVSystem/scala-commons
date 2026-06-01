package com.avsystem.commons
package misc

case class Boxing[-A, +B](fun: A => B) extends AnyVal
object Boxing extends LowPrioBoxing {
  def fromImplicitConv[A, B](using conv: A => B): Boxing[A, B] = Boxing(conv)

  given BooleanBoxing: Boxing[Boolean, JBoolean] = fromImplicitConv
  given ByteBoxing: Boxing[Byte, JByte] = fromImplicitConv
  given ShortBoxing: Boxing[Short, JShort] = fromImplicitConv
  given IntBoxing: Boxing[Int, JInteger] = fromImplicitConv
  given LongBoxing: Boxing[Long, JLong] = fromImplicitConv
  given FloatBoxing: Boxing[Float, JFloat] = fromImplicitConv
  given DoubleBoxing: Boxing[Double, JDouble] = fromImplicitConv
}
trait LowPrioBoxing { this: Boxing.type =>
  given nullableBoxing: [A >: Null] => Boxing[A, A] = Boxing(identity)
}

case class Unboxing[+A, -B](fun: B => A) extends AnyVal
object Unboxing extends LowPrioUnboxing {
  def fromImplicitConv[A, B](using conv: B => A): Unboxing[A, B] = Unboxing(conv)

  given BooleanUnboxing: Unboxing[Boolean, JBoolean] = fromImplicitConv
  given ByteUnboxing: Unboxing[Byte, JByte] = fromImplicitConv
  given ShortUnboxing: Unboxing[Short, JShort] = fromImplicitConv
  given IntUnboxing: Unboxing[Int, JInteger] = fromImplicitConv
  given LongUnboxing: Unboxing[Long, JLong] = fromImplicitConv
  given FloatUnboxing: Unboxing[Float, JFloat] = fromImplicitConv
  given DoubleUnboxing: Unboxing[Double, JDouble] = fromImplicitConv
}
trait LowPrioUnboxing { this: Unboxing.type =>
  given nullableUnboxing: [A >: Null] => Unboxing[A, A] = Unboxing(identity)
}
