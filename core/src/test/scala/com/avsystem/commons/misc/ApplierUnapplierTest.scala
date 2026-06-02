package com.avsystem.commons.misc

import com.avsystem.commons
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

case class Inner(s: String)
case class Outer(inner: Inner, n: Int)
case class WithOpt(o: Option[String], i: Int)

sealed trait Animal
case object Dog extends Animal
case object Cat extends Animal

case class Empty()
case class Single(str: String)
case class Multiple(str: String, int: Int)
case class Varargs(str: String, ints: Int*)
case class Generic[T](str: String, value: T)

case class Over22(
  p01: String = "01",
  p02: String = "02",
  p03: String = "03",
  p04: String = "04",
  p05: String = "05",
  p06: String = "06",
  p07: String = "07",
  p08: String = "08",
  p09: String = "09",
  p10: String = "10",
  p11: String = "11",
  p12: String = "12",
  p13: String = "13",
  p14: String = "14",
  p15: String = "15",
  p16: String = "16",
  p17: String = "17",
  p18: String = "18",
  p19: String = "19",
  p20: String = "20",
  p21: String = "21",
  p22: String = "22",
  p23: String = "23",
  p24: String = "24",
  p25: String = "25",
)

given CustomProductApplierUnapplier: ProductApplierUnapplier[Multiple] = new {
  override def apply(rawValues: Seq[Any]): Multiple =
    Multiple(rawValues.head.asInstanceOf[String], rawValues(1).asInstanceOf[Int])
}

class ApplierUnapplierTest extends AnyFunSuite with Matchers {
  def roundtrip[T: {Applier as applier, Unapplier as unapplier, ApplierUnapplier as applierUnapplier}](
    value: T
  )(using Position
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
  test("tuple") {
    roundtrip(("", 42, 3.14))
  }
  test("nested case class") {
    roundtrip(Outer(Inner("inside"), 7))
  }
  test("case class with Option field") {
    roundtrip(WithOpt(Some("x"), 1))
    roundtrip(WithOpt(None, 2))
  }
  test("sealed trait does not derive") {
    "summon[Applier[Animal]]" shouldNot compile
    "summon[Unapplier[Animal]]" shouldNot compile
    "summon[ApplierUnapplier[Animal]]" shouldNot compile
  }
  test("ProductApplierUnapplier subclass") {
    roundtrip(Multiple("hi", 5))
  }
  test("Applier and Unapplier summon standalone") {
    val a = summon[Applier[Single]]
    val u = summon[Unapplier[Single]]
    val value = Single("solo")
    assert(u.unapply(value) == Seq("solo"))
    assert(a(Seq("solo")) == value)
  }
}
