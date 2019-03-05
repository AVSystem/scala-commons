package com.avsystem.commons
package misc

import org.scalatest.FunSuite

class OptTest extends FunSuite {
  test("nonempty test") {
    val opt = Opt(23)
    opt match {
      case Opt(num) => assert(num == 23)
    }
  }

  test("empty test") {
    val str: String = null
    val opt = Opt(str)
    opt match {
      case Opt.Empty =>
    }
  }

  test("null some test") {
    intercept[NullPointerException](Opt.some[String](null))
  }

  test("boxing unboxing test") {
    val opt: Opt[Int] = Opt(42)
    val boxedOpt: Opt[JInteger] = opt.boxed
    val unboxedOpt: Opt[Int] = boxedOpt.unboxed
    assert(opt == unboxedOpt)
  }

  test("nesting test") {
    val opt: Opt[Opt[String]] = Opt(Opt.empty)
    val result = opt match {
      case Opt.Empty => false
      case Opt(Opt.Empty) => true
    }
    assert(result)
  }

  test("empty hash code") {
    Opt.Empty.hashCode
  }

  test("orNull") {
    assert((Opt.Empty.orNull: Any) == null)
    assert(Opt("").orNull == "")
  }

  test("collect") {
    assert(Opt(3).collect { case 3 => 2 } == Opt(2))
    assert(Opt(3).collect { case 2 => 2 } == Opt.Empty)
    assert(Opt(3).collect { case 3 => null } == Opt.Empty)
  }

  test("zip") {
    assert(Opt(3).zip(Opt(2)) == Opt((3, 2)))
    assert(Opt.Empty.zip(Opt(2)) == Opt.Empty)
    assert(Opt(3).zip(Opt.Empty) == Opt.Empty)
  }
}
