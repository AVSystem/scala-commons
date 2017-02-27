package com.avsystem.commons
package misc

import org.scalatest.{FunSuite, Matchers}

class OptArgTest extends FunSuite with Matchers {
  test("nonempty") {
    val opt = OptArg(23)
    opt match {
      case OptArg(num) => assert(num == 23)
    }
  }

  test("empty") {
    val str: String = null
    val opt = OptArg(str)
    opt match {
      case OptArg.Empty =>
    }
  }

  test("null some") {
    intercept[NullPointerException](OptArg.some[String](null))
  }

  def takeMaybeString(str: OptArg[String] = OptArg.Empty): Opt[String] = str.toOpt

  test("argument passing") {
    takeMaybeString() shouldEqual Opt.Empty
    takeMaybeString("stringzor") shouldEqual "stringzor".opt
  }
}
