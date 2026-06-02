package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SourceInfoTest extends AnyFunSuite with Matchers {
  val srcInfo = SourceInfo.here

  test("simple") {
    // Scala 3 macro `Position.ofMacroExpansion` points to the receiver (start of
    // `SourceInfo.here`) rather than the method name itself, hence different
    // offset/column from the upstream Scala 2.13 macro.
    srcInfo should matchPattern {
      case SourceInfo(
            _,
            "SourceInfoTest.scala",
            205,
            8,
            17,
            "  val srcInfo = SourceInfo.here",
            List("srcInfo", "SourceInfoTest", "misc", "commons", "avsystem", "com"),
          ) =>
    }
  }
}
