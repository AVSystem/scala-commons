package com.avsystem.commons.misc

import org.scalatest.funsuite.AnyFunSuite
import com.avsystem.commons.JInteger

class NOptTest extends AnyFunSuite {
  test("nonempty test") {
    val opt = NOpt(23)
    opt match {
      case NOpt(num) => assert(num == 23)
    }
  }

  test("empty test") {
    val str: String = null
    val opt = NOpt(str)
    opt match {
      case NOpt.Empty =>
    }
  }

  test("null some test") {
    val str: String = null
    val opt = NOpt.some(str)
    opt match {
      case NOpt(null) =>
    }
  }

  test("boxing unboxing test") {
    val opt: NOpt[Int] = NOpt(42)
    val boxedNOpt: NOpt[JInteger] = opt.boxed
    val unboxedNOpt: NOpt[Int] = boxedNOpt.unboxed
    assert(opt == unboxedNOpt)
  }

  test("nesting test") {
    assert((NOpt(NOpt.empty): Any) match {
      case NOpt.Empty => false
      case NOpt(NOpt.Empty) => true
    })
    assert((NOpt.Empty: Any) match {
      case NOpt(NOpt.Empty) => false
      case NOpt.Empty => true
    })
  }

  test("empty hash code") {
    NOpt.Empty.hashCode
  }

  test("collect") {
    assert(NOpt(3).collect { case 2 => 2 } == NOpt.Empty)
    assert(NOpt(3).collect { case 3 => 2 } == NOpt.some(2))
    assert(NOpt(3).collect { case 3 => null } == NOpt.some(null))
  }

  test("zip") {
    assert(NOpt(3).zip(NOpt(2)) == NOpt((3, 2)))
    assert(NOpt.Empty.zip(NOpt(2)) == NOpt.Empty)
    assert(NOpt(3).zip(NOpt.Empty) == NOpt.Empty)
  }
}
