package com.avsystem.commons.misc

import com.avsystem.commons.JInteger
import org.scalatest.funsuite.AnyFunSuite

class OptRefTest extends AnyFunSuite {
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

  test("zip") {
    assert(OptRef[JInteger](3).zip(OptRef[JInteger](2)) == OptRef((3, 2)))
    assert(OptRef.Empty.zip(OptRef[JInteger](2)) == OptRef.Empty)
    assert(OptRef[JInteger](3).zip(OptRef.Empty) == OptRef.Empty)
  }
}
