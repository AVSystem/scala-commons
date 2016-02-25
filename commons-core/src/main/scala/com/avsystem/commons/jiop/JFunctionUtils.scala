package com.avsystem.commons
package jiop

import java.util.concurrent.Callable
import java.util.{Comparator, function => juf}

import com.avsystem.commons.misc.Sam

import scala.language.implicitConversions

/**
  * Utils to convert Scala functions and expressions to most common Java functional interfaces.
  */
trait JFunctionUtils {
  def jRunnable(code: => Any) = Sam[Runnable](code)
  def jCallable[T](expr: => T) = Sam[Callable[T]](expr)
  def jComparator[T](cmp: (T, T) => Int) = Sam[Comparator[T]](cmp)

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

  def jBiConsumer[T, U](code: (T, U) => Any) = Sam[JBiConsumer[T, U]](code)
  def jBiFunction[T, U, R](fun: (T, U) => R) = Sam[JBiFunction[T, U, R]](fun)
  def jBiPredicate[T, U](pred: (T, U) => Boolean) = Sam[JBiPredicate[T, U]](pred)
  def jBinaryOperator[T](op: (T, T) => T) = Sam[JBinaryOperator[T]](op)
  def jBooleanSupplier(expr: => Boolean) = Sam[JBooleanSupplier](expr)
  def jConsumer[T](code: T => Any) = Sam[JConsumer[T]](code)
  def jDoubleBinaryOperator(op: (Double, Double) => Double) = Sam[JDoubleBinaryOperator](op)
  def jDoubleConsumer(code: Double => Any) = Sam[JDoubleConsumer](code)
  def jDoubleFunction[R](fun: Double => R) = Sam[JDoubleFunction[R]](fun)
  def jDoublePredicate(pred: Double => Boolean) = Sam[JDoublePredicate](pred)
  def jDoubleSupplier(expr: => Double) = Sam[JDoubleSupplier](expr)
  def jDoubleToIntFunction(fun: Double => Int) = Sam[JDoubleToIntFunction](fun)
  def jDoubleToLongFunction(fun: Double => Long) = Sam[JDoubleToLongFunction](fun)
  def jDoubleUnaryOperator(op: Double => Double) = Sam[JDoubleUnaryOperator](op)
  def jFunction[T, R](fun: T => R) = Sam[JFunction[T, R]](fun)
  def jIntBinaryOperator(op: (Int, Int) => Int) = Sam[JIntBinaryOperator](op)
  def jIntConsumer(code: Int => Any) = Sam[JIntConsumer](code)
  def jIntFunction[R](fun: Int => R) = Sam[JIntFunction[R]](fun)
  def jIntPredicate(pred: Int => Boolean) = Sam[JIntPredicate](pred)
  def jIntSupplier(expr: => Int) = Sam[JIntSupplier](expr)
  def jIntToDoubleFunction(fun: Int => Double) = Sam[JIntToDoubleFunction](fun)
  def jIntToLongFunction(fun: Int => Long) = Sam[JIntToLongFunction](fun)
  def jIntUnaryOperator(op: Int => Int) = Sam[JIntUnaryOperator](op)
  def jLongBinaryOperator(op: (Long, Long) => Long) = Sam[JLongBinaryOperator](op)
  def jLongConsumer(code: Long => Any) = Sam[JLongConsumer](code)
  def jLongFunction[R](fun: Long => R) = Sam[JLongFunction[R]](fun)
  def jLongPredicate(pred: Long => Boolean) = Sam[JLongPredicate](pred)
  def jLongSupplier(expr: => Long) = Sam[JLongSupplier](expr)
  def jLongToDoubleFunction(fun: Long => Double) = Sam[JLongToDoubleFunction](fun)
  def jLongToIntFunction(fun: Long => Int) = Sam[JLongToIntFunction](fun)
  def jLongUnaryOperator(op: Long => Long) = Sam[JLongUnaryOperator](op)
  def jObjDoubleConsumer[T](code: (T, Double) => Any) = Sam[JObjDoubleConsumer[T]](code)
  def jObjIntConsumer[T](code: (T, Int) => Any) = Sam[JObjIntConsumer[T]](code)
  def jObjLongConsumer[T](code: (T, Long) => Any) = Sam[JObjLongConsumer[T]](code)
  def jPredicate[T](pred: T => Boolean) = Sam[JPredicate[T]](pred)
  def jSupplier[T](expr: => T) = Sam[JSupplier[T]](expr)
  def jToDoubleBiFunction[T, U](fun: (T, U) => Double) = Sam[JToDoubleBiFunction[T, U]](fun)
  def jToDoubleFunction[T](fun: T => Double) = Sam[JToDoubleFunction[T]](fun)
  def jToIntBiFunction[T, U](fun: (T, U) => Int) = Sam[JToIntBiFunction[T, U]](fun)
  def jToIntFunction[T](fun: T => Int) = Sam[JToIntFunction[T]](fun)
  def jToLongBiFunction[T, U](fun: (T, U) => Long) = Sam[JToLongBiFunction[T, U]](fun)
  def jToLongFunction[T](fun: T => Long) = Sam[JToLongFunction[T]](fun)
  def jUnaryOperator[T](op: T => T) = Sam[JUnaryOperator[T]](op)
}
