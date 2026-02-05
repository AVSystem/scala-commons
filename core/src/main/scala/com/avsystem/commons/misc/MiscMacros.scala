package com.avsystem.commons
package misc

import com.avsystem.commons.misc.SamCompanion.ValidSam

import scala.quoted.*

trait AnnotationOfMacros {
  inline implicit def materialize[A, T]: AnnotationOf[A, T] = ${ MiscMacros.materializeAnnotationOf[A, T] }
}
trait OptAnnotationOfMacros {
  inline implicit def materialize[A, T]: OptAnnotationOf[A, T] = ${ MiscMacros.materializeOptAnnotationOf[A, T] }
}
trait AnnotationsOfMacros {
  inline implicit def materialize[A, T]: AnnotationsOf[A, T] = ${ MiscMacros.materializeAnnotationsOf[A, T] }
}
trait SelfAnnotationMacros {
  inline implicit def materialize[A]: SelfAnnotation[A] = ${ MiscMacros.materializeSelfAnnotation[A] }
}
trait SelfOptAnnotationMacros {
  inline implicit def materialize[A]: SelfOptAnnotation[A] = ${ MiscMacros.materializeSelfOptAnnotation[A] }
}
trait SelfAnnotationsMacros {
  inline implicit def materialize[A]: SelfAnnotations[A] = ${ MiscMacros.materializeSelfAnnotations[A] }
}

trait SimpleClassNameMacros {
  inline implicit def materialize[T]: SimpleClassName[T] = ${ MiscMacros.materializeSimpleClassName[T] }
}

trait SourceInfoMacros {
  inline implicit def here: SourceInfo = ${ MiscMacros.materializeSourceInfo }
}

trait ImplicitsMacros {
  inline def infer[T]: T = ${ MiscMacros.inferImpl[T] }
  inline def infer[T](inline clue: String): T = ${ MiscMacros.clueInferImpl[T]('clue) }
  inline def inferNonMacro[T](inline clue: String): T = ${ MiscMacros.inferNonMacroImpl[T]('clue) }
}

trait SelfInstanceMacros {
  inline implicit def materialize[C[_]]: SelfInstance[C] = ???
}
trait SamCompanionMacros[T, F] {
  inline def apply(fun: F): T = ???
}
trait ValidSamMacros {
  inline implicit def isValidSam[T, F]: ValidSam[T, F] = ???
}
trait DelegationMacros {
  inline implicit def materializeDelegation[A, B]: Delegation[A, B] = ???
}
trait DelegationApplyMacros[B] {
  inline def apply[A](inline source: A): B = ???
}
trait SamMacros {
  inline def apply[T](inline fun: => Any): T = ???
}
trait TypeStringMacros {
  inline implicit def materialize[T]: TypeString[T] = ???
}
trait JavaClassNameMacros {
  inline implicit def materialize[T]: JavaClassName[T] = ???
}
trait SealedUtilsMacros {
  inline def caseObjectsFor[T]: List[T] = ???
  inline def instancesFor[TC[_], T]: List[TC[T]] = ???
}
trait UnapplierMacros {
  inline implicit def materialize[T]: Unapplier[T] = ???
}
trait ApplierUnapplierMacros {
  inline implicit def materialize[T]: ApplierUnapplier[T] = ???
}

object MiscMacros {
  def materializeAnnotationOf[A: Type, T: Type](using Quotes): Expr[AnnotationOf[A, T]] = '{ ??? }
  def materializeOptAnnotationOf[A: Type, T: Type](using Quotes): Expr[OptAnnotationOf[A, T]] = '{ ??? }
  def materializeAnnotationsOf[A: Type, T: Type](using Quotes): Expr[AnnotationsOf[A, T]] = '{ ??? }
  def materializeSelfAnnotation[A: Type](using Quotes): Expr[SelfAnnotation[A]] = '{ ??? }
  def materializeSelfOptAnnotation[A: Type](using Quotes): Expr[SelfOptAnnotation[A]] = '{ ??? }
  def materializeSelfAnnotations[A: Type](using Quotes): Expr[SelfAnnotations[A]] = '{ ??? }
  def materializeSimpleClassName[T: Type](using Quotes): Expr[SimpleClassName[T]] = '{ ??? }
  def materializeSourceInfo(using Quotes): Expr[SourceInfo] = '{ ??? }
  def inferImpl[T: Type](using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
  def clueInferImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
  def inferNonMacroImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
}
