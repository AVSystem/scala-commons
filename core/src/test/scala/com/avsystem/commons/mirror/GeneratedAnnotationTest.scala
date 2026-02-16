package com.avsystem.commons
package mirror

import org.scalatest.funsuite.AnyFunSuite

class GeneratedAnnotationTest extends AnyFunSuite {
  import GeneratedAnnotationTest.*

  test("product: @generated members are exposed via GeneratedElems and compute values") {
    val mirror = DerMirror.derived[Prod]

    type Expected = GeneratedDerElem {
      type MirroredType = String
      type MirroredLabel = "ab"
      type Metadata = Meta @generated
      type OuterMirroredType = Prod
    } *: GeneratedDerElem {
      type MirroredType = Int
      type MirroredLabel = "len"
      type Metadata = Meta @generated
      type OuterMirroredType = Prod
    } *: EmptyTuple

    summon[mirror.GeneratedElems =:= Expected]

    val value = Prod(2, "x")
    val gAb *: gLen *: EmptyTuple = mirror.generatedElems
    assert(gAb(value) == "2-x")
    assert(gLen(value) == 1)
  }

  test("value class: @generated members are exposed") {
    val mirror = DerMirror.derived[VC]

    type Expected = GeneratedDerElem {
      type MirroredType = String
      type MirroredLabel = "upper"
      type Metadata = Meta @generated
      type OuterMirroredType = VC
    } *: EmptyTuple

    summon[mirror.GeneratedElems =:= Expected]

    val gUpper *: EmptyTuple = mirror.generatedElems
    assert(gUpper(VC("ab")) == "AB")
  }

  test("sum (sealed trait): @generated members on the trait are exposed") {
    val mirror = DerMirror.derived[SumADT]

    type Expected = GeneratedDerElem {
      type MirroredType = Int
      type MirroredLabel = "const"
      type Metadata = Meta @generated
      type OuterMirroredType = SumADT
    } *: EmptyTuple

    summon[mirror.GeneratedElems =:= Expected]

    val gConst *: EmptyTuple = mirror.generatedElems
    assert(gConst(SumADT.Case1(10)) == 42)
    assert(gConst(SumADT.Case2) == 42)
  }

  test("enum: @generated members on the enum are exposed") {
    val mirror = DerMirror.derived[GenEnum]

    type Expected = GeneratedDerElem {
      type MirroredType = String
      type MirroredLabel = "info"
      type Metadata = Meta @generated
      type OuterMirroredType = GenEnum
    } *: EmptyTuple

    summon[mirror.GeneratedElems =:= Expected]

    val gInfo *: EmptyTuple = mirror.generatedElems
    assert(gInfo(GenEnum.A) == "ENUM")
    assert(gInfo(GenEnum.B(5)) == "ENUM")
  }

  test("singleton object: @generated members are exposed and compute values") {
    val mirror = DerMirror.derived[GenObj.type]

    type Expected = GeneratedDerElem {
      type MirroredType = Int
      type MirroredLabel = "id"
      type Metadata = Meta @generated
      type OuterMirroredType = GenObj.type
    } *: EmptyTuple

    summon[mirror.GeneratedElems =:= Expected]

    val gId *: EmptyTuple = mirror.generatedElems
    assert(gId(GenObj) == 7)
  }
}

object GeneratedAnnotationTest {
  final case class Prod(a: Int, b: String) {
    @generated def ab: String = s"$a-$b"
    @generated def len: Int = b.length
  }
  final case class VC(x: String) extends AnyVal {
    @generated def upper: String = x.toUpperCase
  }
  sealed trait SumADT {
    @generated val const: Int = 42
  }
  object SumADT {
    final case class Case1(i: Int) extends SumADT
    case object Case2 extends SumADT
  }
  enum GenEnum {
    case A
    case B(i: Int)
    @generated val info: String = "ENUM"
  }
  case object GenObj {
    @generated def id: Int = 7
  }
}
