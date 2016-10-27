package com.avsystem.commons
package misc

import com.avsystem.commons.jiop.JavaInterop._
import org.scalatest.FunSuite

class OptRefTest extends FunSuite {
  test("nonempty test") {
    val opt = OptRef("lol")
    opt match {
      case OptRef(str) => assert(str == "lol")
    }
  }

  test("empty test") {
    val str: String = null
    val opt = OptRef(str)
    opt match {
      case OptRef.Empty =>
    }
  }

  test("null some test") {
    intercept[NullPointerException](OptRef.some[String](null))
  }

  test("unboxing matching test") {
    val opt = OptRef[JInteger](42)
    opt match {
      case OptRef.Boxed(num) => assert(num == 42)
    }
  }
}
