package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SourceInfoTest extends AnyFunSuite with Matchers {
  val srcInfo = SourceInfo.here

  test("simple") {
    srcInfo should matchPattern {
      case SourceInfo(
            _,
            "SourceInfoTest.scala",
            216,
            8,
            28,
            "  val srcInfo = SourceInfo.here",
            List("srcInfo", "SourceInfoTest", "misc", "commons", "avsystem", "com"),
          ) =>
    }
  }
}
