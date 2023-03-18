package com.avsystem.commons
package collection

import org.scalatest.funsuite.AnyFunSuite

class MutableStackTest extends AnyFunSuite {
  test("push") {
    val stack = new MutableStack[String]
    stack.push("lol1")
    stack.push("lol2")
    stack.push("lol3")
    assert(stack.asList == List("lol3", "lol2", "lol1"))
  }

  test("pushAll") {
    val stack = new MutableStack[String]
    stack.pushAll(List("lol1", "lol2", "lol3"))
    assert(stack.asList == List("lol3", "lol2", "lol1"))
  }

  test("pop") {
    val stack = new MutableStack[String]
    assertThrows[NoSuchElementException](stack.pop())
    assert(stack.popOpt().isEmpty)
    assert(stack.popOption().isEmpty)
    stack.push("lol")
    assert(stack.pop() == "lol")
    assert(stack.isEmpty)
    stack.push("lol")
    assert(stack.popOpt() == Opt("lol"))
    assert(stack.isEmpty)
    stack.push("lol")
    assert(stack.popOption().contains("lol"))
    assert(stack.isEmpty)
  }

  test("top") {
    val stack = new MutableStack[String]
    assertThrows[NoSuchElementException](stack.top)
    assert(stack.topOpt.isEmpty)
    assert(stack.topOption.isEmpty)
    stack.push("lol")
    assert(stack.top == "lol")
    assert(stack.topOpt.contains("lol"))
    assert(stack.topOption.contains("lol"))
  }

}
