package com.avsystem.commons
package misc

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.serialization.GenCodec

// NOTE: This file contains compile-time type-shape checks only (no ScalaTest methods).
// The Phase-1 /* */ envelope is lifted; declarations are reshaped to satisfy Phase 4 bounds:
//   - MacroInstances second type parameter must satisfy `Instances <: AnyNamedTuple` (slice 4.2)
//   - AdtMetadataCompanion type parameter must satisfy `M[X] <: TypedMetadata[X]` (slice 4.4)
//
// Sections that cannot be reshaped without Phase 6 work (e.g. ComplexInstances mixing val/var/def
// + GenCodec.materialize runtime stub) remain wrapped per [[feedback_stub_over_comment]] —
// these block on real MetaMacros bodies, NOT on type shape.

/* TODO[scala3-port]: ComplexInstancesTest — ComplexInstances mixes val/var/def with
   implicit-parametered method; cannot be expressed as a named tuple type alias.
   Reshape requires Phase 6 (real MetaMacros + GenCodec.materialize bodies).
object ComplexInstancesTest {
  case class Dep(int: Int)
  case class Klass[T](value: T)

  object DependencyImplicits {
    implicit val depCodec: GenCodec[Dep] = GenCodec.materialize
  }

  trait ComplexInstances[T] {
    val plainCodec: GenCodec[Klass[Int]]
    var codecWithGeneric: GenCodec[Klass[T]]
    def dependencyUsingCodec: GenCodec[Klass[Dep]]
    def parameterizedCodec[A: GenCodec]: GenCodec[Klass[A]]
  }

  abstract class HasComplexInstances[T](
    implicit macroInstances: MacroInstances[DependencyImplicits.type, ComplexInstances[T]]
  ) {
    val instances: ComplexInstances[T] = macroInstances(DependencyImplicits, this)
  }
}
 */

object MultipleImplicitImportsTest {
  case class A(str: String)
  case class B(int: Int)

  object ACodec {
    implicit val aCodec: GenCodec[A] = GenCodec.materialize
  }
  object BCodec {
    implicit val bCodec: GenCodec[B] = GenCodec.materialize
  }

  // Reshaped: `() => GenCodec[T]` -> named tuple `(codec: GenCodec[T])` (slice 4.2 bound).
  abstract class HasGenCodecUsingAB[T](implicit instances: MacroInstances[(ACodec.type, BCodec.type), (codec: GenCodec[T])]) {
    implicit lazy val codec: GenCodec[T] = instances((ACodec, BCodec), this).codec
  }

  case class AwithB(a: A, b: B)
  object AwithB extends HasGenCodecUsingAB[AwithB]
}

/* TODO[scala3-port]: AnnotationReferringToEnclosingObjectTest — uses `infer.value`
   as a default argument of an annotation parameter. The fork's `MetaMacros.valueImpl`
   ships `'{ ??? }` (slice 4.3 stub) so the splice cannot synthesize the enclosing-`T`
   `GenCodec` at the annotation call site — type inference falls back to `Nothing`
   and the case-class default-argument resolution fails. Reshape requires Phase 6
   (real reflection-based `valueImpl` body that walks the annotation's owner tree).
   Fork itself wraps this whole file as DISABLED for Scala 3 — we preserve the
   compile-time-type-shape sections that do work under Phase 4 bounds.
object AnnotationReferringToEnclosingObjectTest {
  class example[+T](value: T, @infer codec: GenCodec[T] = infer.value) extends StaticAnnotation

  class Meta[T](@reifyAnnot example: example[T]) extends TypedMetadata[T]
  object Meta extends AdtMetadataCompanion[Meta]

  abstract class HasMeta[T](implicit instances: MacroInstances[Unit, (meta: Meta[T])]) {
    implicit val meta: Meta[T] = instances((), this).meta
  }

  @example(Rec("lol", 42))
  case class Rec(str: String, int: Int)
  object Rec extends HasMeta[Rec] {
    implicit val codec: GenCodec[Rec] = GenCodec.materialize
  }
}
 */
