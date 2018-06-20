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

  val stringzor = "stringzor"
  val returnedMaybeStringzor = stringzor.opt

  test("empty default argument") {
    takeMaybeString() shouldEqual Opt.Empty
  }

  test("argument passing") {
    takeMaybeString(stringzor) shouldEqual returnedMaybeStringzor
  }

  test("Opt argument passing") {
    val opt = stringzor.opt
    val empty = Opt.Empty
    takeMaybeString(opt) shouldEqual returnedMaybeStringzor
    takeMaybeString(empty) shouldEqual Opt.Empty
  }

  test("Option argument passing") {
    val option: Option[String] = stringzor.option
    val some: Some[String] = Some(stringzor)
    val none: None.type = None

    takeMaybeString(option) shouldEqual returnedMaybeStringzor
    takeMaybeString(some) shouldEqual returnedMaybeStringzor
    takeMaybeString(none) shouldEqual Opt.Empty
  }
}
