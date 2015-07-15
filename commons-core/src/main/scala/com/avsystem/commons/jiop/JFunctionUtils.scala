package com.avsystem.commons
package jiop

import java.util.concurrent.Callable
import java.util.{function => juf, Comparator}

import scala.language.implicitConversions

/**
 * Utils to convert Scala functions and expressions to most common Java functional interfaces.
 */
trait JFunctionUtils {
  def jRunnable(code: => Any): Runnable =
    new Runnable {
      def run(): Unit = code
    }

  def jCallable[T](expr: => T): Callable[T] =
    new Callable[T] {
      def call(): T = expr
    }

  def jComparator[T](cmp: (T, T) => Int): Comparator[T] =
    new Comparator[T] {
      def compare(o1: T, o2: T): Int = cmp(o1, o2)
    }

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


  def jBiConsumer[T, U](code: (T, U) => Any): JBiConsumer[T, U] =
    new JBiConsumer[T, U] {
      def accept(left: T, right: U) = code(left, right)
    }

  def jBiFunction[T, U, R](fun: (T, U) => R): JBiFunction[T, U, R] =
    new JBiFunction[T, U, R] {
      def apply(left: T, right: U) = fun(left, right)
    }

  def jBiPredicate[T, U](pred: (T, U) => Boolean): JBiPredicate[T, U] =
    new JBiPredicate[T, U] {
      def test(left: T, right: U) = pred(left, right)
    }

  def jBinaryOperator[T](op: (T, T) => T): JBinaryOperator[T] =
    new JBinaryOperator[T] {
      def apply(left: T, right: T) = op(left, right)
    }

  def jBooleanSupplier(expr: => Boolean): JBooleanSupplier =
    new JBooleanSupplier {
      def getAsBoolean = expr
    }

  def jConsumer[T](code: T => Any): JConsumer[T] =
    new JConsumer[T] {
      def accept(t: T) = code(t)
    }

  def jDoubleBinaryOperator(op: (Double, Double) => Double): JDoubleBinaryOperator =
    new JDoubleBinaryOperator {
      def applyAsDouble(left: Double, right: Double) = op(left, right)
    }

  def jDoubleConsumer(code: Double => Any): JDoubleConsumer =
    new JDoubleConsumer {
      def accept(value: Double) = code(value)
    }

  def jDoubleFunction[R](fun: Double => R): JDoubleFunction[R] =
    new JDoubleFunction[R] {
      def apply(value: Double) = fun(value)
    }

  def jDoublePredicate(pred: Double => Boolean): JDoublePredicate =
    new JDoublePredicate {
      def test(value: Double) = pred(value)
    }

  def jDoubleSupplier(expr: => Double): JDoubleSupplier =
    new JDoubleSupplier {
      def getAsDouble = expr
    }

  def jDoubleToIntFunction(fun: Double => Int): JDoubleToIntFunction =
    new JDoubleToIntFunction {
      def applyAsInt(value: Double) = fun(value)
    }

  def jDoubleToLongFunction(fun: Double => Long): JDoubleToLongFunction =
    new JDoubleToLongFunction {
      def applyAsLong(value: Double) = fun(value)
    }

  def jDoubleUnaryOperator(op: Double => Double): JDoubleUnaryOperator =
    new JDoubleUnaryOperator {
      def applyAsDouble(value: Double) = op(value)
    }

  def jFunction[T, R](fun: T => R): JFunction[T, R] =
    new JFunction[T, R] {
      def apply(t: T) = fun(t)
    }

  def jIntBinaryOperator(op: (Int, Int) => Int): JIntBinaryOperator =
    new JIntBinaryOperator {
      def applyAsInt(left: Int, right: Int) = op(left, right)
    }

  def jIntConsumer(code: Int => Any): JIntConsumer =
    new JIntConsumer {
      def accept(value: Int) = code(value)
    }

  def jIntFunction[R](fun: Int => R): JIntFunction[R] =
    new JIntFunction[R] {
      def apply(value: Int) = fun(value)
    }

  def jIntPredicate(pred: Int => Boolean): JIntPredicate =
    new JIntPredicate {
      def test(value: Int) = pred(value)
    }

  def jIntSupplier(expr: => Int): JIntSupplier =
    new JIntSupplier {
      def getAsInt = expr
    }

  def jIntToDoubleFunction(fun: Int => Double): JIntToDoubleFunction =
    new JIntToDoubleFunction {
      def applyAsDouble(value: Int) = fun(value)
    }

  def jIntToLongFunction(fun: Int => Long): JIntToLongFunction =
    new JIntToLongFunction {
      def applyAsLong(value: Int) = fun(value)
    }

  def jIntUnaryOperator(op: Int => Int): JIntUnaryOperator =
    new JIntUnaryOperator {
      def applyAsInt(value: Int) = op(value)
    }

  def jLongBinaryOperator(op: (Long, Long) => Long): JLongBinaryOperator =
    new JLongBinaryOperator {
      def applyAsLong(left: Long, right: Long) = op(left, right)
    }

  def jLongConsumer(code: Long => Any): JLongConsumer =
    new JLongConsumer {
      def accept(value: Long) = code(value)
    }

  def jLongFunction[R](fun: Long => R): JLongFunction[R] =
    new JLongFunction[R] {
      def apply(value: Long) = fun(value)
    }

  def jLongPredicate(pred: Long => Boolean): JLongPredicate =
    new JLongPredicate {
      def test(value: Long) = pred(value)
    }

  def jLongSupplier(expr: => Long): JLongSupplier =
    new JLongSupplier {
      def getAsLong = expr
    }

  def jLongToDoubleFunction(fun: Long => Double): JLongToDoubleFunction =
    new JLongToDoubleFunction {
      def applyAsDouble(value: Long) = fun(value)
    }

  def jLongToIntFunction(fun: Long => Int): JLongToIntFunction =
    new JLongToIntFunction {
      def applyAsInt(value: Long) = fun(value)
    }

  def jLongUnaryOperator(op: Long => Long): JLongUnaryOperator =
    new JLongUnaryOperator {
      def applyAsLong(value: Long) = op(value)
    }

  def jObjDoubleConsumer[T](code: (T, Double) => Any): JObjDoubleConsumer[T] =
    new JObjDoubleConsumer[T] {
      def accept(t: T, value: Double) = code(t, value)
    }

  def jObjIntConsumer[T](code: (T, Int) => Any): JObjIntConsumer[T] =
    new JObjIntConsumer[T] {
      def accept(t: T, value: Int) = code(t, value)
    }

  def jObjLongConsumer[T](code: (T, Long) => Any): JObjLongConsumer[T] =
    new JObjLongConsumer[T] {
      def accept(t: T, long: Long) = code(t, long)
    }

  def jPredicate[T](pred: T => Boolean): JPredicate[T] =
    new JPredicate[T] {
      def test(t: T) = pred(t)
    }

  def jSupplier[T](expr: => T): JSupplier[T] =
    new JSupplier[T] {
      def get = expr
    }

  def jToDoubleBiFunction[T, U](fun: (T, U) => Double): JToDoubleBiFunction[T, U] =
    new JToDoubleBiFunction[T, U] {
      def applyAsDouble(left: T, right: U) = fun(left, right)
    }

  def jToDoubleFunction[T](fun: T => Double): JToDoubleFunction[T] =
    new JToDoubleFunction[T] {
      def applyAsDouble(t: T) = fun(t)
    }

  def jToIntBiFunction[T, U](fun: (T, U) => Int): JToIntBiFunction[T, U] =
    new JToIntBiFunction[T, U] {
      def applyAsInt(left: T, right: U) = fun(left, right)
    }

  def jToIntFunction[T](fun: T => Int): JToIntFunction[T] =
    new JToIntFunction[T] {
      def applyAsInt(t: T) = fun(t)
    }

  def jToLongBiFunction[T, U](fun: (T, U) => Long): JToLongBiFunction[T, U] =
    new JToLongBiFunction[T, U] {
      def applyAsLong(left: T, right: U) = fun(left, right)
    }

  def jToLongFunction[T](fun: T => Long): JToLongFunction[T] =
    new JToLongFunction[T] {
      def applyAsLong(t: T) = fun(t)
    }

  def jUnaryOperator[T](op: T => T): JUnaryOperator[T] =
    new JUnaryOperator[T] {
      def apply(t: T) = op(t)
    }
}
