package com.avsystem.commons
package meta

import com.avsystem.commons.misc.ImplicitNotFound

import scala.annotation.implicitNotFound

/** Base trait for companion objects of _metadata classes_. A metadata class is a generic class that captures metadata
  * for some Scala type, typically an RPC interface ([[com.avsystem.commons.rpc.RpcMetadataCompanion
  * RpcMetadataCompanion]]) or a data type ([[AdtMetadataCompanion]]).
  *
  * @tparam M
  *   metadata class constructor
  */
trait MetadataCompanion[M[_]] {
  given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real] = fallback.value
  final def apply[Real](using metadata: M[Real]): M[Real] = metadata
  final class Lazy[Real](metadata: => M[Real]) {
    lazy val value: M[Real] = metadata
  }
  object Lazy extends MetadataCompanionLazyMacros[M, Lazy] {
    @implicitNotFound("#{forNotLazy}")
    given notFound: [T] => (forNotLazy: ImplicitNotFound[M[T]]) => ImplicitNotFound[Lazy[T]] =
      ImplicitNotFound()
    def apply[Real](metadata: => M[Real]): Lazy[Real] = new Lazy(metadata)
  }
}

/** Like [[MetadataCompanion]] but allows the metadata class' type parameter to be bounded
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
  given fromFallback: [Real >: Lo <: Hi] => (fallback: Fallback[M[Real]]) => M[Real] = fallback.value
  final def apply[Real >: Lo <: Hi](using metadata: M[Real]): M[Real] = metadata
  final class Lazy[Real >: Lo <: Hi](metadata: => M[Real]) {
    lazy val value: M[Real] = metadata
  }
  object Lazy extends BoundedMetadataCompanionLazyMacros[Hi, Lo, M, Lazy] {
    @implicitNotFound("#{forNotLazy}")
    given notFound: [T >: Lo <: Hi] => (forNotLazy: ImplicitNotFound[M[T]]) => ImplicitNotFound[Lazy[T]] =
      ImplicitNotFound()
    def apply[Real >: Lo <: Hi](metadata: => M[Real]): Lazy[Real] = new Lazy(metadata)
  }
}
