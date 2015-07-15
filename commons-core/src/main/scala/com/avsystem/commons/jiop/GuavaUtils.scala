package com.avsystem.commons
package jiop

import com.google.common.{base => gbase}

trait GuavaUtils {
  type GFunction[F, T] = gbase.Function[F, T]
  type GSupplier[T] = gbase.Supplier[T]
  type GPredicate[T] = gbase.Predicate[T]

  def gFunction[F, T](fun: F => T): GFunction[F, T] =
    new GFunction[F, T] {
      def apply(input: F): T = fun(input)
    }

  def gSupplier[T](expr: => T): GSupplier[T] =
    new GSupplier[T] {
      def get(): T = expr
    }

  def gPredicate[T](pred: T => Boolean): GPredicate[T] =
    new GPredicate[T] {
      def apply(input: T): Boolean = pred(input)
    }
}


