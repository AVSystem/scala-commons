package com.avsystem.commons
package mirror

import org.scalatest.funsuite.AnyFunSuite

class DerMirrorTest extends AnyFunSuite {
  import DerMirrorTest.*

  test("DerMirror for case class") {
    val _: DerMirror {
      type MirroredType = SimpleCaseClass
      type MirroredLabel = "SimpleCaseClass"
      type MirroredElems = DerElem.Of[Long] {
        type MirroredLabel = "id"
        type Metadata = Meta
      } *: DerElem.Of[String] {
        type MirroredLabel = "name"
        type Metadata = Meta
      } *: EmptyTuple
      type Metadata = Meta
    } = DerMirror.derived[SimpleCaseClass]
  }

  test("DerMirror for case class with no fields") {
    val _: DerMirror.Product {
      type MirroredType = NoFields
      type MirroredLabel = "NoFields"
      type Metadata = Meta
      type MirroredElems = Any
    } = DerMirror.derived[NoFields]
  }

  test("DerMirror for generic case class") {
    val _: DerMirror.Product {
      type MirroredType = Box[Int]
      type MirroredLabel = "Box"
      type Metadata = Meta
      type MirroredElems = DerElem.Of[Int] {
        type MirroredLabel = "a"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[Box[Int]]
  }

  test("DerMirror for enum") {
    val _: DerMirror.Sum {
      type MirroredType = SimpleEnum
      type MirroredLabel = "SimpleEnum"
      type Metadata = Meta
      type MirroredElems = DerMirror.Of[SimpleEnum.Case1.type] {
        type MirroredLabel = "Case1"
        type Metadata = Meta
      } *: DerMirror.Of[SimpleEnum.Case2] {
        type MirroredLabel = "Case2"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[SimpleEnum]
  }

  test("DerMirror for object") {
    val mirror: DerMirror.Singleton {
      type MirroredType = SimpleObject.type
      type MirroredLabel = "SimpleObject"
      type Metadata = Meta
      type MirroredElems = Any
    } = DerMirror.derived[SimpleObject.type]

    assert(mirror.value == SimpleObject)
  }

  test("DerMirror for Unit") {
    val mirror: DerMirror.Singleton {
      type MirroredType = Unit
      type MirroredLabel = "Unit"
      type Metadata = Meta
    } = DerMirror.derived[Unit]
    assert(mirror.value == ())
  }

  test("DerMirror for value class") {
    val mirror: DerMirror.Product {
      type MirroredType = ValueClass

      type MirroredLabel = "ValueClass"

      type Metadata = Meta
    } = DerMirror.derived[ValueClass]
    assert(mirror.fromUnsafeArray(Array("test")) == ValueClass("test"))
  }

  test("DerMirror for @transparent case class") {
    val mirror: DerMirror.Transparent {
      type MirroredType = TransparentClass
      type MirroredElemType = Int
      type MirroredLabel = "TransparentClass"
      type Metadata = Meta
    } = DerMirror.derived[TransparentClass]

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

//  test("DerMirror for enum with @name") {
//    val mirror = DerMirror.derived[NamedEnum]
//    summon[mirror.MirroredElemLabels =:= ("C1" *: "Case2" *: EmptyTuple)]
//  }

  test("DerMirror for recursive ADT") {
    val mirror = DerMirror.derived[Recursive]
    summon[mirror.MirroredLabel =:= "Recursive"]
    summon[
      mirror.MirroredElems =:= (
        DerMirror.Of[Recursive.End.type] {
          type MirroredLabel = "End"
          type Metadata = Meta
        } *: DerMirror.Of[Recursive.Next] {
          type MirroredLabel = "Next"
          type Metadata = Meta
        } *: EmptyTuple,
      ),
    ]
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
  case class SimpleCaseClass(id: Long, name: String)
  case class NoFields()
  enum SimpleEnum {
    case Case1
    case Case2(data: String)
  }
  case class ValueClass(str: String) extends AnyVal
  @transparent
  case class TransparentClass(int: Int)
  case class ParamAnnotation(value: String) extends MetaAnnotation
  @ParamAnnotation("foo")
  case class ParamAnnotated(id: Int)
  case class Box[T](a: T)
  enum NamedEnum {
    @name("C1") case Case1
    case Case2
  }
  class Annotation1 extends MetaAnnotation
  class Annotation2 extends MetaAnnotation
  class Annotation3 extends MetaAnnotation
  @Annotation1 @Annotation2
  case class AnnotatedCaseClass(id: Long)
  @Annotation1 @Annotation2 @Annotation3
  case class ManyAnnotated(id: Long)
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
