package com.avsystem.commons.meta

import scala.quoted.*

trait InferMacros {
  inline def value[T]: T = ${ MetaMacros.valueImpl[T] }
}

trait AdtMetadataCompanionMacros[M[_]] {
  inline implicit def materialize[T]: M[T] = ${ MetaMacros.dummy }
  inline def fromApplyUnapplyProvider[T](inline applyUnapplyProvider: Any): M[T] =
    ${ MetaMacros.dummy }
}

trait BoundedAdtMetadataCompanionMacros[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] {
  inline def materialize[T >: Lo <: Hi]: M[T] = ${ MetaMacros.dummy }
  inline def fromApplyUnapplyProvider[T >: Lo <: Hi](inline applyUnapplyProvider: Any): M[T] =
    ${ MetaMacros.dummy }
}

trait MetadataCompanionMacros[M[_]] {
  inline def materialize[T]: M[T] = ${ MetaMacros.dummy }
  inline def fromApplyUnapplyProvider[T](inline applyUnapplyProvider: Any): M[T] =
    ${ MetaMacros.dummy }
}

trait BoundedMetadataCompanionMacros[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] {
  inline def materialize[T >: Lo <: Hi]: M[T] = ${ MetaMacros.dummy }
  inline def fromApplyUnapplyProvider[T >: Lo <: Hi](inline applyUnapplyProvider: Any): M[T] =
    ${ MetaMacros.dummy }
}

trait MetadataCompanionLazyMacros[M[_], Lazy[_]] {
  inline implicit def lazyMetadata[Real](implicit metadata: M[Real]): Lazy[Real] = ${ MetaMacros.lazyMetadataImpl }
}

trait BoundedMetadataCompanionLazyMacros[Hi, Lo <: Hi, M[_ >: Lo <: Hi], Lazy[_ >: Lo <: Hi]] {
  inline implicit def lazyMetadata[Real >: Lo <: Hi](implicit metadata: M[Real]): Lazy[Real] = ${
    MetaMacros.lazyMetadataImpl
  }
}

object MetaMacros {
  def valueImpl[T: Type](using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
  def lazyMetadataImpl(using Quotes): Expr[Nothing] = '{ ??? }

  def dummy(using Quotes): Expr[Nothing] = '{ ??? }
}
