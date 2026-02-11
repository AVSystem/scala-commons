package com.avsystem.commons
package mirror

import com.avsystem.commons.mirror.DerMirror.{Transparent, getAnnot}
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

  test("DerMirror for Unit") {
    val mirror = DerMirror.derived[Unit]
    summon[mirror.MirroredType =:= Unit]
    summon[mirror.MirroredMonoType =:= Unit]
    summon[mirror.MirroredLabel =:= "Unit"]
    assert(mirror.value == ())
  }

  test("DerMirror for value class") {

    val mirror = DerMirror.derived[ValueClass]
    summon[mirror.MirroredLabel =:= "ValueClass"]
    summon[mirror.MirroredElemLabels =:= ("str" *: EmptyTuple)]
    summon[mirror.MirroredElemTypes =:= (String *: EmptyTuple)]

    val vc = ValueClass("test")
    assert(mirror.unwrap(vc) == "test")
    assert(mirror.wrap("test") == vc)
  }

  test("DerMirror for @transparent case class") {
    val mirror = DerMirror.derived[TransparentClass]
    summon[mirror.MirroredLabel =:= "TransparentClass"]
    summon[mirror.MirroredElemLabels =:= ("int" *: EmptyTuple)]
    summon[mirror.MirroredElemTypes =:= (Int *: EmptyTuple)]

    val tc = TransparentClass(42)
    assert(mirror.unwrap(tc) == 42)
    assert(mirror.wrap(42) == tc)
  }

  test("getAnnotation and hasAnnotation") {
    val mirror = DerMirror.derived[AnnotatedCaseClass]
    summon[mirror.Metadata =:= (Meta @Annotation2 @Annotation1)]

    assert(mirror.hasAnnotation[Annotation1])
    assert(mirror.hasAnnotation[Annotation2])
    assert(!mirror.hasAnnotation[Annotation3])

    assert(mirror.getAnnotation[Annotation1].isDefined)
    assert(mirror.getAnnotation[Annotation2].isDefined)
    assert(mirror.getAnnotation[Annotation3].isEmpty)
  }

  test("getAnnotation and hasAnnotation for extended annotations") {
    val mirror = DerMirror.derived[InheritedAnnotatedCaseClass]
    assert(mirror.hasAnnotation[Annotation1])
    assert(mirror.getAnnotation[Annotation1].isDefined)
  }

  test("parametrized annotation") {
    val mirror = DerMirror.derived[ParamAnnotated]
    val annot = mirror.getAnnotation[ParamAnnotation].get
    assert(annot.value == "foo")
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

//  test("DerMirror for ADT with mixed cases") {
//    val mirror = DerMirror.derived[MixedADT]
//    summon[mirror.MirroredLabel =:= "MixedADT"]
//    summon[mirror.MirroredElemLabels =:= ("CaseObj" *: "CaseClass" *: EmptyTuple)]
//    summon[mirror.Metadata =:= Meta]
//  }
}

object DerMirrorTest {
  sealed trait MixedADT
  trait AnnotatedTrait {
    @Annotation1 def annotatedMethod: String
  }
  case class SimpleCaseClass(id: Long, name: String)
  case class NoFields()
  enum SimpleEnum {
    case Case1
    case Case2(data: String)
  }
  @transparent
  case class ValueClass(str: String) extends AnyVal
  @transparent
  case class TransparentClass(int: Int)
  case class ParamAnnotation(value: String) extends MetaAnnotation
  @ParamAnnotation("foo")
  case class ParamAnnotated(id: Int)
  case class Box[T](a: T)
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
  case class InheritedAnnotatedCaseClass(annotatedMethod: String)
  enum Recursive {
    case End
    case Next(r: Recursive)
  }
  case object SimpleObject
  object MixedADT {
    case class CaseClass(v: Int) extends MixedADT
    case object CaseObj extends MixedADT
  }
}
