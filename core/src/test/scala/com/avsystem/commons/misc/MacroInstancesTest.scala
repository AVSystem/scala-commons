package com.avsystem.commons
package misc

import com.avsystem.commons.meta.{AdtMetadataCompanion, MacroInstances, infer, reifyAnnot}
import com.avsystem.commons.serialization.GenCodec

object ComplexInstancesTest {
  abstract class HasComplexInstances[T](
    using macroInstances: MacroInstances[
      DependencyImplicits.type,
      (
        plainCodec: GenCodec[Klass[Int]],
        codecWithGeneric: GenCodec[Klass[T]],
        dependencyUsingCodec: GenCodec[Klass[Dep]],
        parameterizedCodec: [A: GenCodec] => () => GenCodec[Klass[A]],
      ),
    ],
  ) {
    val instances = macroInstances(DependencyImplicits, this)
  }
  case class Dep(int: Int)
  case class Klass[T](value: T)
  object DependencyImplicits {
    given GenCodec[Dep] = GenCodec.materialize
  }
}

object MultipleImplicitImportsTest {
  abstract class HasGenCodecUsingAB[T](
    using instances: MacroInstances[(ACodec.type, BCodec.type), (codec: GenCodec[T])],
  ) {
    given GenCodec[T] = instances((ACodec, BCodec), this).codec
  }
  case class A(str: String)
  case class B(int: Int)
  case class AwithB(a: A, b: B)
  object ACodec {
    given GenCodec[A] = GenCodec.materialize
  }
  object BCodec {
    given GenCodec[B] = GenCodec.materialize
  }
  object AwithB extends HasGenCodecUsingAB[AwithB]
}

object AnnotationReferringToEnclosingObjectTest {
  abstract class HasMeta[T](
    using instances: MacroInstances[Unit, (meta: Meta[T])],
  ) {
    given Meta[T] = instances((), this).meta
  }
  class example[+T](value: T, @infer codec: GenCodec[T] = infer.value[GenCodec[T]]) extends StaticAnnotation
  // object Meta extends AdtMetadataCompanion[Meta]
  class Meta[T](@reifyAnnot example: example[T])
  @example(Rec("lol", 42))
  case class Rec(str: String, int: Int)
  object Rec extends HasMeta[Rec] {
    given GenCodec[Rec] = GenCodec.materialize
  }
}
