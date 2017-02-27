package com.avsystem.commons
package misc

import org.scalatest.{FunSuite, Matchers}

class SourceInfoTest extends FunSuite with Matchers {
  val srcInfo = SourceInfo.here

  test("simple") {
    srcInfo should matchPattern {
      case SourceInfo(_, "SourceInfoTest.scala", 167, 7, 28,
      "  val srcInfo = SourceInfo.here",
      List("srcInfo", "SourceInfoTest", "misc", "commons", "avsystem", "com")) =>
    }
  }
}
