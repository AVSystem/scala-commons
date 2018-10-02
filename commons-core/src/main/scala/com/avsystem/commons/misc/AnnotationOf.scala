package com.avsystem.commons
package misc

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
