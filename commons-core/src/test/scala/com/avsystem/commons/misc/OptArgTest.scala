package com.avsystem.commons.misc

import com.avsystem.commons.SharedExtensions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class OptArgTest extends AnyFunSuite with Matchers {
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
