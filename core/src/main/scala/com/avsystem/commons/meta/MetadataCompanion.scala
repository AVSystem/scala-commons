package com.avsystem.commons
package meta

import com.avsystem.commons.macros.misc.MiscMacros
import com.avsystem.commons.misc.ImplicitNotFound

import scala.annotation.implicitNotFound

/**
 * Base trait for companion objects of _metadata classes_. A metadata class is a generic class that captures metadata
 * for some Scala type, typically an RPC interface ([[com.avsystem.commons.rpc.RpcMetadataCompanion
 * RpcMetadataCompanion]]) or a data type ([[AdtMetadataCompanion]]).
 *
 * @tparam M
 *   metadata class constructor
 */
trait MetadataCompanion[M[_]] {
  final def apply[Real](implicit metadata: M[Real]): M[Real] = metadata

  implicit final def fromFallback[Real](implicit fallback: Fallback[M[Real]]): M[Real] = fallback.value

  final class Lazy[Real](metadata: => M[Real]) {
    lazy val value: M[Real] = metadata
  }
  object Lazy extends MetadataCompanionLazyMacros[M, Lazy] {
    def apply[Real](metadata: => M[Real]): Lazy[Real] = new Lazy(metadata)

    @implicitNotFound("#{forNotLazy}")
    implicit def notFound[T](implicit forNotLazy: ImplicitNotFound[M[T]]): ImplicitNotFound[Lazy[T]] =
      ImplicitNotFound()
  }
}


/**
 * Like [[MetadataCompanion]] but allows the metadata class' type parameter to be bounded
 *
 * @tparam Hi
 *   higher bound of metadata class' type param
 * @tparam Lo
 *   lower bound of metadata class' type param
 * @tparam M
 *   metadata class type constructor
 */
// cannot share code with MetadataCompanion because of binary compatibility problems, must copy
trait BoundedMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] {
  final def apply[Real >: Lo <: Hi](implicit metadata: M[Real]): M[Real] = metadata

  implicit final def fromFallback[Real >: Lo <: Hi](implicit fallback: Fallback[M[Real]]): M[Real] = fallback.value

  final class Lazy[Real >: Lo <: Hi](metadata: => M[Real]) {
    lazy val value: M[Real] = metadata
  }
  object Lazy extends BoundedMetadataCompanionLazyMacros[Hi, Lo, M, Lazy] {
    def apply[Real >: Lo <: Hi](metadata: => M[Real]): Lazy[Real] = new Lazy(metadata)

    @implicitNotFound("#{forNotLazy}")
    implicit def notFound[T >: Lo <: Hi](implicit forNotLazy: ImplicitNotFound[M[T]]): ImplicitNotFound[Lazy[T]] =
      ImplicitNotFound()
  }
}
