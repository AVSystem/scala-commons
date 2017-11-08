package com.avsystem.commons
package misc

import org.scalatest.FunSuite

class CrossUtilsTest extends FunSuite {
  final val isJvm = System.getProperty("java.home") != null

  test("backend-conditional compilation test") {
    assert(CrossUtils.cross("JVM", "JS") == (if (isJvm) "JVM" else "JS"))
  }
}
