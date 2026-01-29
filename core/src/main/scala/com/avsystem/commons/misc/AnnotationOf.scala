package com.avsystem.commons
package misc

import scala.annotation.{implicitNotFound, publicInBinary, RefiningAnnotation}

/**
 * A typeclass which captures an annotation of type `A` applied on a class/trait/object associated with type `T`. If
 * this annotation is absent, compilation will fail.
 * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
 */
@implicitNotFound("${T} is not annotated with ${A}")
case class AnnotationOf[A, T](annot: A) extends AnyVal
object AnnotationOf extends AnnotationOfMacros {}

/**
 * A typeclass which captures a possible annotation of type `A` applied on a class/trait/object associated with type
 * `T`. [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]]
 * apply.
 */
case class OptAnnotationOf[A, T](annotOpt: Opt[A])
object OptAnnotationOf extends OptAnnotationOfMacros {}

/**
 * A typeclass which captures all annotations of type `A` applied on a class/trait/object associated with type `T`.
 * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
 */
case class AnnotationsOf[A, T](annots: List[A]) extends AnyVal
object AnnotationsOf extends AnnotationsOfMacros {}

/**
 * A typeclass which serves as an evidence that an annotation of type `A` is applied on a class/trait/object associated
 * with type `T`.
 * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
 * Similar to [[AnnotationOf]] but does not reify the annotation itself into runtime.
 */
@implicitNotFound("${T} is not annotated with ${A}")
final class HasAnnotation[A <: RefiningAnnotation, T](val annotation: A)
object HasAnnotation {
  transparent inline given [A <: RefiningAnnotation, T] => HasAnnotation[A, T] = ${ materializeImpl[A, T] }
  private def materializeImpl[A <: RefiningAnnotation: Type, T: Type](using quotes: Quotes): Expr[HasAnnotation[A, T]] = {
    import quotes.reflect._
    TypeRepr.of[T].typeSymbol.getAnnotation(TypeRepr.of[A].typeSymbol) match {
      case Some(annot) => '{ new HasAnnotation(${ annot.asExprOf[A] }) }
      case _ => report.errorAndAbort(s"${Type.show[T]} is not annotated with ${Type.show[A]}")
    }
  }

}

/**
 * A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures an annotation of
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
object SelfAnnotation extends SelfAnnotationMacros {}

/**
 * A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures a possible
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
object SelfOptAnnotation extends SelfOptAnnotationMacros {}

/**
 * A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures all annotations of
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
object SelfAnnotations extends SelfAnnotationsMacros {}
