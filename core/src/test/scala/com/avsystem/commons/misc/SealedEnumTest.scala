package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite

class SealedEnumTest extends AnyFunSuite {
  sealed abstract class SomeEnum(implicit val sourceInfo: SourceInfo) extends OrderedEnum
  object SomeEnum extends SealedEnumCompanion[SomeEnum] {
    case object First extends SomeEnum
    case object Second extends SomeEnum
    case object Third extends SomeEnum
    case object Fourth extends SomeEnum

    val values: List[SomeEnum] = caseObjects
    val classTags: List[ClassTag[? <: SomeEnum]] = SealedUtils.instancesFor[ClassTag, SomeEnum]
  }

  test("case objects listing") {
    import SomeEnum.*
    assert(values == List(First, Second, Third, Fourth))
  }

  test("typeclass instance listing") {
    import SomeEnum.*
    assert(classTags.map(_.runtimeClass) == List(First.getClass, Second.getClass, Third.getClass, Fourth.getClass))
  }
}
