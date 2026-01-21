package com.avsystem.commons
package jiop

import java.util.{function => juf}

import com.avsystem.commons.misc.Sam

/** Utils to convert Scala functions and expressions to most common Java functional interfaces.
  */
trait JFunctionUtils {
  type JBiConsumer[T, U] = juf.BiConsumer[T, U]
  type JBiFunction[T, U, R] = juf.BiFunction[T, U, R]
  type JBiPredicate[T, U] = juf.BiPredicate[T, U]
  type JBinaryOperator[T] = juf.BinaryOperator[T]
  type JBooleanSupplier = juf.BooleanSupplier
  type JConsumer[T] = juf.Consumer[T]
  type JDoubleBinaryOperator = juf.DoubleBinaryOperator
  type JDoubleConsumer = juf.DoubleConsumer
  type JDoubleFunction[R] = juf.DoubleFunction[R]
  type JDoublePredicate = juf.DoublePredicate
  type JDoubleSupplier = juf.DoubleSupplier
  type JDoubleToIntFunction = juf.DoubleToIntFunction
  type JDoubleToLongFunction = juf.DoubleToLongFunction
  type JDoubleUnaryOperator = juf.DoubleUnaryOperator
  type JFunction[T, R] = juf.Function[T, R]
  type JIntBinaryOperator = juf.IntBinaryOperator
  type JIntConsumer = juf.IntConsumer
  type JIntFunction[R] = juf.IntFunction[R]
  type JIntPredicate = juf.IntPredicate
  type JIntSupplier = juf.IntSupplier
  type JIntToDoubleFunction = juf.IntToDoubleFunction
  type JIntToLongFunction = juf.IntToLongFunction
  type JIntUnaryOperator = juf.IntUnaryOperator
  type JLongBinaryOperator = juf.LongBinaryOperator
  type JLongConsumer = juf.LongConsumer
  type JLongFunction[R] = juf.LongFunction[R]
  type JLongPredicate = juf.LongPredicate
  type JLongSupplier = juf.LongSupplier
  type JLongToDoubleFunction = juf.LongToDoubleFunction
  type JLongToIntFunction = juf.LongToIntFunction
  type JLongUnaryOperator = juf.LongUnaryOperator
  type JObjDoubleConsumer[T] = juf.ObjDoubleConsumer[T]
  type JObjIntConsumer[T] = juf.ObjIntConsumer[T]
  type JObjLongConsumer[T] = juf.ObjLongConsumer[T]
  type JPredicate[T] = juf.Predicate[T]
  type JSupplier[T] = juf.Supplier[T]
  type JToDoubleBiFunction[T, U] = juf.ToDoubleBiFunction[T, U]
  type JToDoubleFunction[T] = juf.ToDoubleFunction[T]
  type JToIntBiFunction[T, U] = juf.ToIntBiFunction[T, U]
  type JToIntFunction[T] = juf.ToIntFunction[T]
  type JToLongBiFunction[T, U] = juf.ToLongBiFunction[T, U]
  type JToLongFunction[T] = juf.ToLongFunction[T]
  type JUnaryOperator[T] = juf.UnaryOperator[T]

  def jBiConsumer[T, U](code: (T, U) => Any): JBiConsumer[T,U] = Sam[JBiConsumer[T, U]](code)
  def jBiFunction[T, U, R](fun: (T, U) => R): JBiFunction[T,U,R] = Sam[JBiFunction[T, U, R]](fun)
  def jBiPredicate[T, U](pred: (T, U) => Boolean): JBiPredicate[T,U] = Sam[JBiPredicate[T, U]](pred)
  def jBinaryOperator[T](op: (T, T) => T): JBinaryOperator[T] = Sam[JBinaryOperator[T]](op)
  def jBooleanSupplier(expr: => Boolean): JBooleanSupplier = Sam[JBooleanSupplier](expr)
  def jConsumer[T](code: T => Any): JConsumer[T] = Sam[JConsumer[T]](code)
  def jDoubleBinaryOperator(op: (Double, Double) => Double): JDoubleBinaryOperator = Sam[JDoubleBinaryOperator](op)
  def jDoubleConsumer(code: Double => Any): JDoubleConsumer = Sam[JDoubleConsumer](code)
  def jDoubleFunction[R](fun: Double => R): JDoubleFunction[R] = Sam[JDoubleFunction[R]](fun)
  def jDoublePredicate(pred: Double => Boolean): JDoublePredicate = Sam[JDoublePredicate](pred)
  def jDoubleSupplier(expr: => Double): JDoubleSupplier = Sam[JDoubleSupplier](expr)
  def jDoubleToIntFunction(fun: Double => Int): JDoubleToIntFunction = Sam[JDoubleToIntFunction](fun)
  def jDoubleToLongFunction(fun: Double => Long): JDoubleToLongFunction = Sam[JDoubleToLongFunction](fun)
  def jDoubleUnaryOperator(op: Double => Double): JDoubleUnaryOperator = Sam[JDoubleUnaryOperator](op)
  def jFunction[T, R](fun: T => R): JFunction[T,R] = Sam[JFunction[T, R]](fun)
  def jIntBinaryOperator(op: (Int, Int) => Int): JIntBinaryOperator = Sam[JIntBinaryOperator](op)
  def jIntConsumer(code: Int => Any): JIntConsumer = Sam[JIntConsumer](code)
  def jIntFunction[R](fun: Int => R): JIntFunction[R] = Sam[JIntFunction[R]](fun)
  def jIntPredicate(pred: Int => Boolean): JIntPredicate = Sam[JIntPredicate](pred)
  def jIntSupplier(expr: => Int): JIntSupplier = Sam[JIntSupplier](expr)
  def jIntToDoubleFunction(fun: Int => Double): JIntToDoubleFunction = Sam[JIntToDoubleFunction](fun)
  def jIntToLongFunction(fun: Int => Long): JIntToLongFunction = Sam[JIntToLongFunction](fun)
  def jIntUnaryOperator(op: Int => Int): JIntUnaryOperator = Sam[JIntUnaryOperator](op)
  def jLongBinaryOperator(op: (Long, Long) => Long): JLongBinaryOperator = Sam[JLongBinaryOperator](op)
  def jLongConsumer(code: Long => Any): JLongConsumer = Sam[JLongConsumer](code)
  def jLongFunction[R](fun: Long => R): JLongFunction[R] = Sam[JLongFunction[R]](fun)
  def jLongPredicate(pred: Long => Boolean): JLongPredicate = Sam[JLongPredicate](pred)
  def jLongSupplier(expr: => Long): JLongSupplier = Sam[JLongSupplier](expr)
  def jLongToDoubleFunction(fun: Long => Double): JLongToDoubleFunction = Sam[JLongToDoubleFunction](fun)
  def jLongToIntFunction(fun: Long => Int): JLongToIntFunction = Sam[JLongToIntFunction](fun)
  def jLongUnaryOperator(op: Long => Long): JLongUnaryOperator = Sam[JLongUnaryOperator](op)
  def jObjDoubleConsumer[T](code: (T, Double) => Any): JObjDoubleConsumer[T] = Sam[JObjDoubleConsumer[T]](code)
  def jObjIntConsumer[T](code: (T, Int) => Any): JObjIntConsumer[T] = Sam[JObjIntConsumer[T]](code)
  def jObjLongConsumer[T](code: (T, Long) => Any): JObjLongConsumer[T] = Sam[JObjLongConsumer[T]](code)
  def jPredicate[T](pred: T => Boolean): JPredicate[T] = Sam[JPredicate[T]](pred)
  def jSupplier[T](expr: => T): JSupplier[T] = Sam[JSupplier[T]](expr)
  def jToDoubleBiFunction[T, U](fun: (T, U) => Double): JToDoubleBiFunction[T,U] = Sam[JToDoubleBiFunction[T, U]](fun)
  def jToDoubleFunction[T](fun: T => Double): JToDoubleFunction[T] = Sam[JToDoubleFunction[T]](fun)
  def jToIntBiFunction[T, U](fun: (T, U) => Int): JToIntBiFunction[T,U] = Sam[JToIntBiFunction[T, U]](fun)
  def jToIntFunction[T](fun: T => Int): JToIntFunction[T] = Sam[JToIntFunction[T]](fun)
  def jToLongBiFunction[T, U](fun: (T, U) => Long): JToLongBiFunction[T,U] = Sam[JToLongBiFunction[T, U]](fun)
  def jToLongFunction[T](fun: T => Long): JToLongFunction[T] = Sam[JToLongFunction[T]](fun)
  def jUnaryOperator[T](op: T => T): JUnaryOperator[T] = Sam[JUnaryOperator[T]](op)
}
