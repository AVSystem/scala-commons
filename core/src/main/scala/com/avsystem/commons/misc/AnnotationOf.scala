package com.avsystem.commons
package misc

import com.avsystem.commons.macros

import scala.annotation.implicitNotFound

/** A typeclass which captures an annotation of type `A` applied on a class/trait/object associated with type `T`. If
  * this annotation is absent, compilation will fail.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  */
@implicitNotFound("${T} is not annotated with ${A}")
case class AnnotationOf[A, T](annot: A) extends AnyVal
object AnnotationOf {
  implicit def materialize[A, T]: AnnotationOf[A, T] = macro macros.misc.MiscMacros.annotationOf[A, T]

  inline def materialize[A, T]: AnnotationOf[A, T] = ${materializeImpl[A, T]}
  def materializeImpl[A, T](using Quotes): Expr[AnnotationOf[A, T]] = '{???}
}

/** A typeclass which captures a possible annotation of type `A` applied on a class/trait/object associated with type
  * `T`. [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]]
  * apply.
  */
case class OptAnnotationOf[A, T](annotOpt: Opt[A])
object OptAnnotationOf {
  implicit def materialize[A, T]: OptAnnotationOf[A, T] = macro macros.misc.MiscMacros.optAnnotationOf[A, T]
  inline def materialize[A, T]: OptAnnotationOf[A, T] = ${materializeImpl[A, T]}
  def materializeImpl[A, T](using Quotes): Expr[OptAnnotationOf[A, T]] = '{???}
}

/** A typeclass which captures all annotations of type `A` applied on a class/trait/object associated with type `T`.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  */
case class AnnotationsOf[A, T](annots: List[A]) extends AnyVal
object AnnotationsOf {
  implicit def materialize[A, T]: AnnotationsOf[A, T] = macro macros.misc.MiscMacros.annotationsOf[A, T]
  inline def materialize[A, T]: AnnotationOf[A, T] = ${materializeImpl[A, T]}
  def materializeImpl[A, T](using Quotes): Expr[AnnotationOf[A, T]] = '{???}
}

/** A typeclass which serves as an evidence that an annotation of type `A` is applied on a class/trait/object associated
  * with type `T`.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  * Similar to [[AnnotationOf]] but does not reify the annotation itself into runtime.
  */
@implicitNotFound("${T} is not annotated with ${A}")
final class HasAnnotation[A, T] private ()
object HasAnnotation {
  private val reusable = new HasAnnotation
  def create[A, T]: HasAnnotation[A, T] = reusable.asInstanceOf[HasAnnotation[A, T]]

  implicit def materialize[A, T]: HasAnnotation[A, T] = macro macros.misc.MiscMacros.hasAnnotation[A, T]
  inline def materialize[A, T]: HasAnnotation[A, T] = ${materializeImpl[A, T]}
  def materializeImpl[A, T](using Quotes): Expr[HasAnnotation[A, T]] = '{???}
}

/** A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures an annotation of
  * type `A` applied on a class or object which extends this abstract class. If this annotation is absent, compilation
  * will fail. [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]]
  * apply.
  *
  * @example
  *   {{{
  *   final class awesome(value: Boolean) extends scala.annotation.Annotation
  *
  *   abstract class Base(implicit awesomeAnnot: SelfAnnotation[awesome]) {
  *     def isAwesome: Boolean = awesomeAnnot.annot.value
  *   }
  *
  *   @awesome(true)
  *   class AwesomeSubclass extends Base
  *   }}}
  */
case class SelfAnnotation[A](annot: A) extends AnyVal
object SelfAnnotation {
  implicit def materialize[A]: SelfAnnotation[A] = macro macros.misc.MiscMacros.selfAnnotation[A]
  inline def materialize[A]: SelfAnnotation[A] = ${materializeImpl[A]}
  def materializeImpl[A](using Quotes): Expr[SelfAnnotation[A]] = '{???}
}

/** A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures a possible
  * annotation of type `A` applied on a class or object which extends this abstract class.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  *
  * @example
  *   {{{
  *   final class awesome(value: Boolean) extends scala.annotation.Annotation
  *
  *   abstract class Base(implicit awesomeAnnot: SelfOptAnnotation[awesome]) {
  *     def isAwesome: Boolean = awesomeAnnot.annotOpt.exists(_.value)
  *   }
  *
  *   class NotAwesomeSubclass extends Base
  *   @awesome(true) class AwesomeSubclass extends Base
  *   @awesome(false) class ExplicitlyNotAwesomeSubclass extends Base
  *   }}}
  */
case class SelfOptAnnotation[A](annotOpt: Opt[A])
object SelfOptAnnotation {
  implicit def materialize[A]: SelfOptAnnotation[A] = macro macros.misc.MiscMacros.selfOptAnnotation[A]
  inline def materialize[A]: SelfOptAnnotation[A]= ${materializeImpl[A]}
  def materializeImpl[A](using Quotes): Expr[SelfOptAnnotation[A]] = '{???}
}

/** A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures all annotations of
  * type `A` applied on a class or object which extends this abstract class.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  *
  * @example
  *   {{{
  *   final class tag(value: String) extends scala.annotation.Annotation
  *
  *   abstract class Base(implicit tagAnnots: SelfAnnotations[tag]) {
  *     def tags: List[String] = tagAnnots.map(_.value)
  *   }
  *
  *   @tag("t1") @tag("t2") @tag("t3")
  *   class TaggedSubclass extends Base
  *   }}}
  */
case class SelfAnnotations[A](annots: List[A]) extends AnyVal
object SelfAnnotations {
  implicit def materialize[A]: SelfAnnotations[A] = macro macros.misc.MiscMacros.selfAnnotations[A]
  inline def materialize[A]: SelfAnnotations[A] = ${materializeImpl[A]}
  def materializeImpl[A](using Quotes): Expr[SelfAnnotations[A]] = '{???}
}
