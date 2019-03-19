package com.avsystem.commons
package misc

import scala.annotation.implicitNotFound

@implicitNotFound("${T} is not annotated with ${A}")
case class AnnotationOf[A, T](annot: A) extends AnyVal
object AnnotationOf {
  implicit def materialize[A, T]: AnnotationOf[A, T] = macro macros.misc.MiscMacros.annotationOf[A, T]
}

case class OptAnnotationOf[A, T](annotOpt: Opt[A])
object OptAnnotationOf {
  implicit def materialize[A, T]: OptAnnotationOf[A, T] = macro macros.misc.MiscMacros.optAnnotationOf[A, T]
}

case class AnnotationsOf[A, T](annots: List[A]) extends AnyVal
object AnnotationsOf {
  implicit def materialize[A, T]: AnnotationsOf[A, T] = macro macros.misc.MiscMacros.annotationsOf[A, T]
}

@implicitNotFound("${T} is not annotated with ${A}")
final class HasAnnotation[A, T]
object HasAnnotation {
  private[this] val reusable = new HasAnnotation
  def create[A, T]: HasAnnotation[A, T] = reusable.asInstanceOf

  implicit def materialize[A, T]: HasAnnotation[A, T] = macro macros.misc.MiscMacros.hasAnnotation[A, T]
}

case class SelfAnnotation[A](annot: A) extends AnyVal
object SelfAnnotation {
  implicit def materialize[A]: SelfAnnotation[A] = macro macros.misc.MiscMacros.selfAnnotation[A]
}

case class SelfOptAnnotation[A](annotOpt: Opt[A])
object SelfOptAnnotation {
  implicit def materialize[A]: SelfOptAnnotation[A] = macro macros.misc.MiscMacros.selfOptAnnotation[A]
}

case class SelfAnnotations[A](annots: List[A]) extends AnyVal
object SelfAnnotations {
  implicit def materialize[A]: SelfAnnotations[A] = macro macros.misc.MiscMacros.selfAnnotations[A]
}
