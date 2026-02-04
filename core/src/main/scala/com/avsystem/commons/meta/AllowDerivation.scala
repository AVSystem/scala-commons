package com.avsystem.commons.meta

sealed trait AllowDerivation[T]
object AllowDerivation {
  private val reusable = new AllowDerivation[Any] {}
  def create[T]: AllowDerivation[T] = reusable.asInstanceOf[AllowDerivation[T]]
}

object AllowRecursiveDerivation
