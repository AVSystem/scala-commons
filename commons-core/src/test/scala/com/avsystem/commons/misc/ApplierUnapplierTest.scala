package com.avsystem.commons.misc

import com.avsystem.commons
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite

case class Empty()
case class Single(str: String)
case class Multiple(str: String, int: Int)
case class Varargs(str: String, ints: Int*)
case class Generic[T](str: String, value: T)

case class Over22(
  p01: String = "01", p02: String = "02", p03: String = "03", p04: String = "04", p05: String = "05",
  p06: String = "06", p07: String = "07", p08: String = "08", p09: String = "09", p10: String = "10",
  p11: String = "11", p12: String = "12", p13: String = "13", p14: String = "13", p15: String = "15",
  p16: String = "16", p17: String = "17", p18: String = "18", p19: String = "18", p20: String = "18",
  p21: String = "21", p22: String = "22", p23: String = "23", p24: String = "24", p25: String = "25"
)

class Custom(val str: String, val i: Int) {
  override def hashCode(): Int = (str, i).hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case c: Custom => str == c.str && i == c.i
    case _ => false
  }
}
object Custom {
  def apply(str: String, i: Int): Custom = new Custom(str, i)
  def unapply(custom: Custom): commons.Opt[(String, Int)] = Opt((custom.str, custom.i))
}

class ApplierUnapplierTest extends AnyFunSuite {
  def roundtrip[T](value: T)(implicit
    applier: Applier[T], unapplier: Unapplier[T], applierUnapplier: ApplierUnapplier[T], pos: Position
  ): Unit = {
    assert(value == applier(unapplier.unapply(value)))
    assert(value == applierUnapplier(applierUnapplier.unapply(value)))
  }

  test("no params") {
    roundtrip(Empty())
  }
  test("single param") {
    roundtrip(Single(""))
  }
  test("multiple params") {
    roundtrip(Multiple("", 42))
  }
  test("varargs") {
    roundtrip(Varargs("", 1, 2, 3))
  }
  test("generic") {
    roundtrip(Generic("a", "b"))
  }
  test("more than 22 params") {
    roundtrip(Over22())
  }
  test("custom") {
    roundtrip(Custom("", 42))
  }
  test("tuple") {
    roundtrip(("", 42, 3.14))
  }
}
