package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SealedEnumTest extends AnyFunSuite with Matchers {
  sealed abstract class SomeEnum(implicit val sourceInfo: SourceInfo) extends OrderedEnum
  object SomeEnum extends SealedEnumCompanion[SomeEnum] {
    case object First extends SomeEnum
    case object Second extends SomeEnum
    case object Third extends SomeEnum
    case object Fourth extends SomeEnum

    val values: List[SomeEnum] = caseObjects
    val classTags: List[ClassTag[? <: SomeEnum]] = SealedUtils.instancesFor[ClassTag, SomeEnum]
  }

  // Bare sum type used by direct SealedUtils.caseObjects / instancesFor tests (no companion shell).
  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color

  // Nested sum — outer has a case object and a sealed sub-branch with its own case objects.
  sealed trait Shape
  case object Circle extends Shape
  sealed trait Polygon extends Shape
  case object Square extends Polygon
  case object Triangle extends Polygon

  // Mixed: case objects + case classes. caseObjects must skip the case classes.
  sealed trait Mixed
  case object MixedA extends Mixed
  case object MixedB extends Mixed
  case class MixedClass(value: Int) extends Mixed

  test("case objects listing") {
    import SomeEnum.*
    assert(values == List(First, Second, Third, Fourth))
  }

  test("typeclass instance listing") {
    import SomeEnum.*
    assert(classTags.map(_.runtimeClass) == List(First.getClass, Second.getClass, Third.getClass, Fourth.getClass))
  }

  test("SealedUtils.caseObjects works standalone (no companion shell)") {
    SealedUtils.caseObjects[Color] should contain theSameElementsAs List(Red, Green, Blue)
  }

  test("SealedUtils.caseObjects recurses into nested sealed sub-branches") {
    SealedUtils.caseObjects[Shape] should contain theSameElementsAs List(Circle, Square, Triangle)
  }

  test("SealedUtils.caseObjects skips case classes in mixed hierarchies") {
    SealedUtils.caseObjects[Mixed] should contain theSameElementsAs List(MixedA, MixedB)
  }

  test("OrderedEnum.ordering sorts by source declaration order") {
    import SomeEnum.*
    val shuffled = List(Third, First, Fourth, Second)
    shuffled.sorted shouldEqual List(First, Second, Third, Fourth)
  }

  test("instancesFor / caseObjects do not compile for non-sealed types") {
    "SealedUtils.caseObjects[String]" shouldNot typeCheck
    "SealedUtils.instancesFor[ClassTag, String]" shouldNot typeCheck
  }
}
