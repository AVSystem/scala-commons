package com.avsystem.commons
package misc

import com.avsystem.commons.macros
import com.avsystem.commons.misc.SamCompanion.ValidSam

trait AnnotationOfMacros {
  implicit def materialize[A, T]: AnnotationOf[A, T] = macro macros.misc.MiscMacros.annotationOf[A, T]
}
trait OptAnnotationOfMacros {
  implicit def materialize[A, T]: OptAnnotationOf[A, T] = macro macros.misc.MiscMacros.optAnnotationOf[A, T]
}
trait AnnotationsOfMacros {
  implicit def materialize[A, T]: AnnotationsOf[A, T] = macro macros.misc.MiscMacros.annotationsOf[A, T]
}
trait HasAnnotationMacros {
  implicit def materialize[A, T]: HasAnnotation[A, T] = macro macros.misc.MiscMacros.hasAnnotation[A, T]
}
trait SelfAnnotationMacros {
  implicit def materialize[A]: SelfAnnotation[A] = macro macros.misc.MiscMacros.selfAnnotation[A]
}
trait SelfOptAnnotationMacros {
  implicit def materialize[A]: SelfOptAnnotation[A] = macro macros.misc.MiscMacros.selfOptAnnotation[A]
}
trait SelfAnnotationsMacros {
  implicit def materialize[A]: SelfAnnotations[A] = macro macros.misc.MiscMacros.selfAnnotations[A]
}

trait SimpleClassNameMacros {
  implicit def materialize[T]: SimpleClassName[T] = macro macros.misc.MiscMacros.simpleClassName[T]
}

trait SourceInfoMacros {
  implicit def here: SourceInfo = macro macros.misc.MiscMacros.sourceInfo
}

trait ImplicitsMacros {
  def infer[T]: T = macro macros.misc.MiscMacros.infer[T]
  def infer[T](clue: String): T = macro macros.misc.MiscMacros.clueInfer[T]
  def inferNonMacro[T](clue: String): T = macro macros.misc.MiscMacros.inferNonMacro[T]
}

trait ValueEnumMacros {
  protected final class ValName(val valName: String)

  protected implicit def valName: ValName = macro macros.misc.MiscMacros.enumValName
}
trait SelfInstanceMacros {
  implicit def materialize[C[_]]: SelfInstance[C] = macro macros.misc.MiscMacros.selfInstance[C[Any]]
}
trait SamCompanionMacros[T, F] {
  def apply(fun: F): T = macro com.avsystem.commons.macros.misc.SamMacros.toSam[T, F]
}
trait ValidSamMacros {
  implicit def isValidSam[T, F]: ValidSam[T, F] = macro com.avsystem.commons.macros.misc.SamMacros.validateSam[T, F]
}
trait DelegationMacros {
  implicit def materializeDelegation[A, B]: Delegation[A, B] =
    macro com.avsystem.commons.macros.misc.DelegationMacros.materializeDelegation[A, B]
}
trait DelegationApplyMacros[B] {
  def apply[A](source: A): B = macro com.avsystem.commons.macros.misc.DelegationMacros.delegate[A, B]
}
trait SamMacros {
  def apply[T](fun: => Any): T = macro com.avsystem.commons.macros.misc.SamMacros.createSam[T]
}
trait TypeStringMacros {
  implicit def materialize[T]: TypeString[T] = macro macros.misc.MiscMacros.typeString[T]
}
trait JavaClassNameMacros {
  implicit def materialize[T]: JavaClassName[T] = macro macros.misc.MiscMacros.javaClassName[T]
}
trait SealedUtilsMacros {
  def caseObjectsFor[T]: List[T] = macro macros.misc.SealedMacros.caseObjectsFor[T]
  def instancesFor[TC[_], T]: List[TC[T]] = macro macros.misc.SealedMacros.instancesFor[TC[Any], T]
}
trait ApplierMacros {
  implicit def materialize[T]: Applier[T] = macro macros.misc.MiscMacros.applier[T]
}
trait UnapplierMacros {
  implicit def materialize[T]: Unapplier[T] = macro macros.misc.MiscMacros.unapplier[T]
}
trait ApplierUnapplierMacros {
  implicit def materialize[T]: ApplierUnapplier[T] = macro macros.misc.MiscMacros.applierUnapplier[T]
}
