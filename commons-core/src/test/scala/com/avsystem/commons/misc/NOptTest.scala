package com.avsystem.commons
package misc

import com.avsystem.commons.jiop.JavaInterop._
import org.scalatest.FunSuite

/**
  * Author: ghik
  * Created: 08/01/16.
  */
class NOptTest extends FunSuite {
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
    val opt: NOpt[NOpt[String]] = NOpt(NOpt.empty)
    opt match {
      case NOpt(NOpt.Empty) =>
    }
  }

  test("empty hash code") {
    NOpt.Empty.hashCode
  }
}
