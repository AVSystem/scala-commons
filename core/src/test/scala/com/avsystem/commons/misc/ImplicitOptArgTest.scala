package com.avsystem.commons.misc

import com.avsystem.commons.SharedExtensions.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

final class ImplicitOptArgTest extends AnyFunSuite with Matchers {
  test("nonempty") {
    val opt = ImplicitOptArg(23)
    opt match {
      case ImplicitOptArg(num) => assert(num == 23)
    }
  }

  test("empty") {
    val str: String = null
    val opt = ImplicitOptArg(str)
    opt match {
      case ImplicitOptArg.Empty =>
    }
  }

  test("null some") {
    intercept[NullPointerException](ImplicitOptArg.some[String](null))
  }

  def takeMaybeString(implicit str: ImplicitOptArg[String] = ImplicitOptArg.Empty): Opt[String] = str.toOpt
  def takeMaybeInt(implicit str: ImplicitOptArg[Int]): Opt[Int] = str.toOpt

  implicit def int: Int = 42


  test("argument passing") {
    takeMaybeString shouldEqual Opt.Empty
    takeMaybeString(using ImplicitOptArg.Empty) shouldEqual Opt.Empty
    takeMaybeString(using "stringzor") shouldEqual "stringzor".opt

    takeMaybeInt shouldEqual 42.opt
    takeMaybeInt(ImplicitOptArg.Empty) shouldEqual Opt.Empty

    "stefan" |> { implicit st =>
      takeMaybeString shouldEqual "stefan".opt
      takeMaybeString(using "mietek") shouldEqual "mietek".opt
      takeMaybeString(using ImplicitOptArg.Empty) shouldEqual Opt.Empty
    }
  }
}
