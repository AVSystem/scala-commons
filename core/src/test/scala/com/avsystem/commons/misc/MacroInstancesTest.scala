package com.avsystem.commons
package misc

import com.avsystem.commons.meta.{AdtMetadataCompanion, MacroInstances, infer, reifyAnnot}
import com.avsystem.commons.serialization.GenCodec

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
    implicit macroInstances: MacroInstances[DependencyImplicits.type, ComplexInstances[T]],
  ) {
    val instances: ComplexInstances[T] = macroInstances(DependencyImplicits, this)
  }
}

object MultipleImplicitImportsTest {
  case class A(str: String)
  case class B(int: Int)

  object ACodec {
    implicit val aCodec: GenCodec[A] = GenCodec.materialize
  }
  object BCodec {
    implicit val bCodec: GenCodec[B] = GenCodec.materialize
  }

  abstract class HasGenCodecUsingAB[T](
    implicit instances: MacroInstances[(ACodec.type, BCodec.type), () => GenCodec[T]],
  ) {
    implicit lazy val codec: GenCodec[T] = instances((ACodec, BCodec), this).apply()
  }

  case class AwithB(a: A, b: B)
  object AwithB extends HasGenCodecUsingAB[AwithB]
}

object AnnotationReferringToEnclosingObjectTest {
  class example[+T](value: T, @infer codec: GenCodec[T] = infer.value[GenCodec[T]]) extends StaticAnnotation

  class Meta[T](@reifyAnnot example: example[T])
  // object Meta extends AdtMetadataCompanion[Meta]

  abstract class HasMeta[T](
    implicit instances: MacroInstances[Unit, () => Meta[T]],
  ) {
    implicit val meta: Meta[T] = instances((), this).apply()
  }

  @example(Rec("lol", 42))
  case class Rec(str: String, int: Int)
  object Rec extends HasMeta[Rec] {
    implicit val codec: GenCodec[Rec] = GenCodec.materialize
  }
}
