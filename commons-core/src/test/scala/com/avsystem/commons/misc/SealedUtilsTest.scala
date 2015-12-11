package com.avsystem.commons
package misc

import org.scalatest.FunSuite

class SealedUtilsTest extends FunSuite {
  sealed trait SomeEnum
  object SomeEnum extends SomeEnumValues {
    case object First extends SomeEnum
    case object Second extends SomeEnum
    case object Third extends SomeEnum
    case object Fourth extends SomeEnum
  }

  trait SomeEnumValues {
    lazy val values = SealedUtils.caseObjectsFor[SomeEnum]
  }

  test("case objects listing test") {
    import SomeEnum._
    assert(values.toSet == Set(First, Second, Third, Fourth))
  }
}
