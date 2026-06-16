package com.avsystem.commons.misc

import org.scalatest.funsuite.AnyFunSuite

class SimpleClassNameTest extends AnyFunSuite {
  test("String") {
    assert(SimpleClassName.of[String] == "String")
  }

  test("List[Int]") {
    assert(SimpleClassName.of[List[Int]] == "List")
  }

  test("nested case class") {
    assert(SimpleClassName.of[SimpleClassNameTest.Foo] == "Foo")
  }
}

object SimpleClassNameTest {
  case class Foo(x: Int)
}
