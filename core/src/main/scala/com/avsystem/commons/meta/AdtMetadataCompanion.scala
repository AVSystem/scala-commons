package com.avsystem.commons
package meta

/** Base trait for companion objects of ADT metadata classes. ADT means "algebraic data type" which is, in practice, a
  * case class or sealed hierarchy.
  *
  * Metadata class is a generic class that captures some metadata about a particular Scala type, using compile-time
  * reflection. This is done with the `materialize` macro. The macro is steered by various meta-annotations used in the
  * definition of the metadata class, e.g. [[adtParamMetadata]].
  *
  * @tparam M
  *   metadata class constructor
  */
trait AdtMetadataCompanion[M[_]] extends MetadataCompanion[M] {
  // TODO[scala3-port]: materialize (Scala 2 macro def) (L)
  def materialize[T]: M[T] = ???

  // TODO[scala3-port]: fromApplyUnapplyProvider (Scala 2 macro def) (L)
  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): M[T] = ???
}

/** Like [[AdtMetadataCompanion]] but allows the metadata class' type parameter to be bounded.
  *
  * @tparam Hi
  *   higher bound of metadata class' type param
  * @tparam Lo
  *   lower bound of metadata class' type param
  * @tparam M
  *   metadata class type constructor
  */
// cannot share code with AdtMetadataCompanion because of binary compatibility problems, must copy
trait BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] extends BoundedMetadataCompanion[Hi, Lo, M] {
  // TODO[scala3-port]: materialize (bounded) (Scala 2 macro def) (L)
  def materialize[T >: Lo <: Hi]: M[T] = ???

  // TODO[scala3-port]: fromApplyUnapplyProvider (bounded) (Scala 2 macro def) (L)
  def fromApplyUnapplyProvider[T >: Lo <: Hi](applyUnapplyProvider: Any): M[T] = ???
}
