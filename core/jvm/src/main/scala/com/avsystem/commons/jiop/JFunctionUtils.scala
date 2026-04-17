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

  def jBiConsumer[T, U](code: (T, U) => Any): JBiConsumer[T, U] = (t, u) => { code(t, u); () }
  def jBiFunction[T, U, R](fun: (T, U) => R): JBiFunction[T, U, R] = fun(_, _)
  def jBiPredicate[T, U](pred: (T, U) => Boolean): JBiPredicate[T, U] = pred(_, _)
  def jBinaryOperator[T](op: (T, T) => T): JBinaryOperator[T] = op(_, _)
  def jBooleanSupplier(expr: => Boolean): JBooleanSupplier = () => expr
  def jConsumer[T](code: T => Any): JConsumer[T] = t => { code(t); () }
  def jDoubleBinaryOperator(op: (Double, Double) => Double): JDoubleBinaryOperator = op(_, _)
  def jDoubleConsumer(code: Double => Any): JDoubleConsumer = d => { code(d); () }
  def jDoubleFunction[R](fun: Double => R): JDoubleFunction[R] = fun(_)
  def jDoublePredicate(pred: Double => Boolean): JDoublePredicate = pred(_)
  def jDoubleSupplier(expr: => Double): JDoubleSupplier = () => expr
  def jDoubleToIntFunction(fun: Double => Int): JDoubleToIntFunction = fun(_)
  def jDoubleToLongFunction(fun: Double => Long): JDoubleToLongFunction = fun(_)
  def jDoubleUnaryOperator(op: Double => Double): JDoubleUnaryOperator = op(_)
  def jFunction[T, R](fun: T => R): JFunction[T, R] = fun(_)
  def jIntBinaryOperator(op: (Int, Int) => Int): JIntBinaryOperator = op(_, _)
  def jIntConsumer(code: Int => Any): JIntConsumer = i => { code(i); () }
  def jIntFunction[R](fun: Int => R): JIntFunction[R] = fun(_)
  def jIntPredicate(pred: Int => Boolean): JIntPredicate = pred(_)
  def jIntSupplier(expr: => Int): JIntSupplier = () => expr
  def jIntToDoubleFunction(fun: Int => Double): JIntToDoubleFunction = fun(_)
  def jIntToLongFunction(fun: Int => Long): JIntToLongFunction = fun(_)
  def jIntUnaryOperator(op: Int => Int): JIntUnaryOperator = op(_)
  def jLongBinaryOperator(op: (Long, Long) => Long): JLongBinaryOperator = op(_, _)
  def jLongConsumer(code: Long => Any): JLongConsumer = l => { code(l); () }
  def jLongFunction[R](fun: Long => R): JLongFunction[R] = fun(_)
  def jLongPredicate(pred: Long => Boolean): JLongPredicate = pred(_)
  def jLongSupplier(expr: => Long): JLongSupplier = () => expr
  def jLongToDoubleFunction(fun: Long => Double): JLongToDoubleFunction = fun(_)
  def jLongToIntFunction(fun: Long => Int): JLongToIntFunction = fun(_)
  def jLongUnaryOperator(op: Long => Long): JLongUnaryOperator = op(_)
  def jObjDoubleConsumer[T](code: (T, Double) => Any): JObjDoubleConsumer[T] = (t, d) => { code(t, d); () }
  def jObjIntConsumer[T](code: (T, Int) => Any): JObjIntConsumer[T] = (t, i) => { code(t, i); () }
  def jObjLongConsumer[T](code: (T, Long) => Any): JObjLongConsumer[T] = (t, l) => { code(t, l); () }
  def jPredicate[T](pred: T => Boolean): JPredicate[T] = pred(_)
  def jSupplier[T](expr: => T): JSupplier[T] = () => expr
  def jToDoubleBiFunction[T, U](fun: (T, U) => Double): JToDoubleBiFunction[T, U] = fun(_, _)
  def jToDoubleFunction[T](fun: T => Double): JToDoubleFunction[T] = fun(_)
  def jToIntBiFunction[T, U](fun: (T, U) => Int): JToIntBiFunction[T, U] = fun(_, _)
  def jToIntFunction[T](fun: T => Int): JToIntFunction[T] = fun(_)
  def jToLongBiFunction[T, U](fun: (T, U) => Long): JToLongBiFunction[T, U] = fun(_, _)
  def jToLongFunction[T](fun: T => Long): JToLongFunction[T] = fun(_)
  def jUnaryOperator[T](op: T => T): JUnaryOperator[T] = op(_)
}
