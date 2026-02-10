package com.avsystem.commons
package mirror

import org.scalatest.funsuite.AnyFunSuite

class DerMirrorTest extends AnyFunSuite {
  import DerMirrorTest.*

  test("DerMirror for case class") {
    val mirror = DerMirror.derived[SimpleCaseClass]
    summon[mirror.MirroredType =:= SimpleCaseClass]
    summon[mirror.MirroredMonoType =:= SimpleCaseClass]
    summon[mirror.MirroredLabel =:= "SimpleCaseClass"]
    summon[mirror.MirroredElemLabels =:= ("id" *: "name" *: EmptyTuple)]
    summon[mirror.MirroredElemTypes =:= (Long *: String *: EmptyTuple)]
    summon[mirror.Metadata =:= Meta]
  }

  test("DerMirror for case class with no fields") {
    val mirror = DerMirror.derived[NoFields]
    summon[mirror.MirroredType =:= NoFields]
    summon[mirror.MirroredMonoType =:= NoFields]
    summon[mirror.MirroredLabel =:= "NoFields"]
    summon[mirror.MirroredElemLabels =:= EmptyTuple]
    summon[mirror.MirroredElemTypes =:= EmptyTuple]
    summon[mirror.Metadata =:= Meta]
  }

  test("DerMirror for generic case class") {
    val mirror = DerMirror.derived[Box[Int]]
    summon[mirror.MirroredType =:= Box[Int]]
    summon[mirror.MirroredMonoType =:= Box[Int]]
    summon[mirror.MirroredLabel =:= "Box"]
    summon[mirror.MirroredElemLabels =:= ("a" *: EmptyTuple)]
    summon[mirror.MirroredElemTypes =:= (Int *: EmptyTuple)]
    summon[mirror.Metadata =:= Meta]
  }

  test("DerMirror for enum") {
    val mirror = DerMirror.derived[SimpleEnum]
    summon[mirror.MirroredType =:= SimpleEnum]
    summon[mirror.MirroredMonoType =:= SimpleEnum]
    summon[mirror.MirroredLabel =:= "SimpleEnum"]
    summon[mirror.MirroredElemLabels =:= ("Case1" *: "Case2" *: EmptyTuple)]
    summon[mirror.MirroredElemTypes =:= (SimpleEnum.Case1.type *: SimpleEnum.Case2 *: EmptyTuple)]
    summon[mirror.Metadata =:= Meta]
  }

  test("DerMirror for object") {
    val mirror = DerMirror.derived[SimpleObject.type]
    summon[mirror.MirroredType =:= SimpleObject.type]
    summon[mirror.MirroredMonoType =:= SimpleObject.type]
    summon[mirror.MirroredLabel =:= "SimpleObject"]
    summon[mirror.MirroredElemLabels =:= EmptyTuple]
    summon[mirror.MirroredElemTypes =:= EmptyTuple]
    summon[mirror.Metadata =:= Meta]
    assert(mirror.value == SimpleObject)
  }

  test("DerMirror with annotations") {
    val mirror = DerMirror.derived[AnnotatedCaseClass]
    summon[mirror.MirroredLabel =:= "AnnotatedCaseClass"]
    summon[mirror.Metadata =:= (Meta @Annotation2 @Annotation1)]
  }

  test("DerMirror with many annotations") {
    val mirror = DerMirror.derived[ManyAnnotated]
    summon[mirror.Metadata =:= (Meta @Annotation3 @Annotation2 @Annotation1)]
  }

  test("DerMirror for recursive ADT") {
    val mirror = DerMirror.derived[Recursive]
    summon[mirror.MirroredLabel =:= "Recursive"]
    summon[mirror.MirroredElemLabels =:= ("End" *: "Next" *: EmptyTuple)]
  }

  test("DerMirror for ADT with mixed cases") {
    val mirror = DerMirror.derived[MixedADT]
    summon[mirror.MirroredLabel =:= "MixedADT"]
    summon[mirror.MirroredElemLabels =:= ("CaseObj" *: "CaseClass" *: EmptyTuple)]
    summon[mirror.Metadata =:= Meta]
  }
}

object DerMirrorTest {
  case class SimpleCaseClass(id: Long, name: String)
  case class NoFields()

  enum SimpleEnum {
    case Case1
    case Case2(data: String)
  }

  case object SimpleObject

  case class Box[T](a: T)

  sealed trait MixedADT
  object MixedADT {
    case object CaseObj extends MixedADT
    case class CaseClass(v: Int) extends MixedADT
  }

  class Annotation1 extends MetaAnnotation
  class Annotation2 extends MetaAnnotation
  class Annotation3 extends MetaAnnotation

  @Annotation1
  @Annotation2
  case class AnnotatedCaseClass(id: Long)

  @Annotation1
  @Annotation2
  @Annotation3
  case class ManyAnnotated(id: Long)

  enum Recursive {
    case End
    case Next(r: Recursive)
  }
}
