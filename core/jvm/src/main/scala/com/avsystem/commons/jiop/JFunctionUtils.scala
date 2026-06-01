package com.avsystem.commons
package jiop

import java.util.function as juf

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

  inline def jBiConsumer[T, U](inline code: (T, U) => Any): JBiConsumer[T, U] = (t, u) => code(t, u)
  inline def jBiFunction[T, U, R](inline fun: (T, U) => R): JBiFunction[T, U, R] = fun(_, _)
  inline def jBiPredicate[T, U](inline pred: (T, U) => Boolean): JBiPredicate[T, U] = pred(_, _)
  inline def jBinaryOperator[T](inline op: (T, T) => T): JBinaryOperator[T] = op(_, _)
  inline def jBooleanSupplier(expr: => Boolean): JBooleanSupplier = () => expr
  inline def jConsumer[T](inline code: T => Any): JConsumer[T] = t => code(t)
  inline def jDoubleBinaryOperator(inline op: (Double, Double) => Double): JDoubleBinaryOperator = op(_, _)
  inline def jDoubleConsumer(inline code: Double => Any): JDoubleConsumer = d => code(d)
  inline def jDoubleFunction[R](inline fun: Double => R): JDoubleFunction[R] = fun(_)
  inline def jDoublePredicate(inline pred: Double => Boolean): JDoublePredicate = pred(_)
  inline def jDoubleSupplier(expr: => Double): JDoubleSupplier = () => expr
  inline def jDoubleToIntFunction(inline fun: Double => Int): JDoubleToIntFunction = fun(_)
  inline def jDoubleToLongFunction(inline fun: Double => Long): JDoubleToLongFunction = fun(_)
  inline def jDoubleUnaryOperator(inline op: Double => Double): JDoubleUnaryOperator = op(_)
  inline def jFunction[T, R](inline fun: T => R): JFunction[T, R] = fun(_)
  inline def jIntBinaryOperator(inline op: (Int, Int) => Int): JIntBinaryOperator = op(_, _)
  inline def jIntConsumer(inline code: Int => Any): JIntConsumer = i => code(i)
  inline def jIntFunction[R](inline fun: Int => R): JIntFunction[R] = fun(_)
  inline def jIntPredicate(inline pred: Int => Boolean): JIntPredicate = pred(_)
  inline def jIntSupplier(expr: => Int): JIntSupplier = () => expr
  inline def jIntToDoubleFunction(inline fun: Int => Double): JIntToDoubleFunction = fun(_)
  inline def jIntToLongFunction(inline fun: Int => Long): JIntToLongFunction = fun(_)
  inline def jIntUnaryOperator(inline op: Int => Int): JIntUnaryOperator = op(_)
  inline def jLongBinaryOperator(inline op: (Long, Long) => Long): JLongBinaryOperator = op(_, _)
  inline def jLongConsumer(inline code: Long => Any): JLongConsumer = l => code(l)
  inline def jLongFunction[R](inline fun: Long => R): JLongFunction[R] = fun(_)
  inline def jLongPredicate(inline pred: Long => Boolean): JLongPredicate = pred(_)
  inline def jLongSupplier(expr: => Long): JLongSupplier = () => expr
  inline def jLongToDoubleFunction(inline fun: Long => Double): JLongToDoubleFunction = fun(_)
  inline def jLongToIntFunction(inline fun: Long => Int): JLongToIntFunction = fun(_)
  inline def jLongUnaryOperator(inline op: Long => Long): JLongUnaryOperator = op(_)
  inline def jObjDoubleConsumer[T](inline code: (T, Double) => Any): JObjDoubleConsumer[T] = (t, d) => code(t, d)
  inline def jObjIntConsumer[T](inline code: (T, Int) => Any): JObjIntConsumer[T] = (t, i) => code(t, i)
  inline def jObjLongConsumer[T](inline code: (T, Long) => Any): JObjLongConsumer[T] = (t, l) => code(t, l)
  inline def jPredicate[T](inline pred: T => Boolean): JPredicate[T] = pred(_)
  inline def jSupplier[T](expr: => T): JSupplier[T] = () => expr
  inline def jToDoubleBiFunction[T, U](inline fun: (T, U) => Double): JToDoubleBiFunction[T, U] = fun(_, _)
  inline def jToDoubleFunction[T](inline fun: T => Double): JToDoubleFunction[T] = fun(_)
  inline def jToIntBiFunction[T, U](inline fun: (T, U) => Int): JToIntBiFunction[T, U] = fun(_, _)
  inline def jToIntFunction[T](inline fun: T => Int): JToIntFunction[T] = fun(_)
  inline def jToLongBiFunction[T, U](inline fun: (T, U) => Long): JToLongBiFunction[T, U] = fun(_, _)
  inline def jToLongFunction[T](inline fun: T => Long): JToLongFunction[T] = fun(_)
  inline def jUnaryOperator[T](inline op: T => T): JUnaryOperator[T] = op(_)
}
