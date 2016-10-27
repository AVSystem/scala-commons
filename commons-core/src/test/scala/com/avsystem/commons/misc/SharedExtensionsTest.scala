package com.avsystem.commons
package misc

import org.scalatest.{FunSuite, Matchers}

class SharedExtensionsTest extends FunSuite with Matchers {
  test("groupToMap") {
    List.range(0, 10).groupToMap(_ % 3, _.toString) shouldEqual
      Map(0 -> List("0", "3", "6", "9"), 1 -> List("1", "4", "7"), 2 -> List("2", "5", "8"))
  }

  test("maxOpt") {
    List.range(0, 10).maxOpt shouldEqual Opt(9)
    List.empty[Int].maxOpt shouldEqual Opt.Empty
  }
}
