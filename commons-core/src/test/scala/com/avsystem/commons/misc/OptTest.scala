package com.avsystem.commons
package misc

import com.avsystem.commons
import org.scalatest.funsuite.AnyFunSuite

class OptTest extends AnyFunSuite {
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
    val opt: commons.Opt[Int] = Opt(42)
    val boxedOpt: commons.Opt[JInteger] = opt.boxed
    val unboxedOpt: commons.Opt[Int] = boxedOpt.unboxed
    assert(opt == unboxedOpt)
  }

  test("nesting test") {
    assert((Opt(Opt.empty): Any) match {
      case Opt.Empty => false
      case Opt(Opt.Empty) => true
    })
    assert((Opt.Empty: Any) match {
      case Opt(Opt.Empty) => false
      case Opt.Empty => true
    })
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

  test("mapOr") {
    val seq: Seq[Int] = Opt(5).mapOr(Nil, i => 0 until i)
    assert(seq == (0 until 5))
  }
}
