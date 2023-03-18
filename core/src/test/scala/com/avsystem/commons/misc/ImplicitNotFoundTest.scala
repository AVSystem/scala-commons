package com.avsystem.commons
package misc

import com.avsystem.commons.testutil.CompilationErrorAssertions
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.implicitNotFound

sealed trait Stuff
object Stuff {
  @implicitNotFound("no stuff available")
  implicit def inf: ImplicitNotFound[Stuff] = ImplicitNotFound()
}

sealed trait OtherStuff
object OtherStuff {
  @implicitNotFound("no other stuff available because: #{forStuff}")
  implicit def inf(implicit forStuff: ImplicitNotFound[Stuff]): ImplicitNotFound[OtherStuff] = ImplicitNotFound()
}

class ImplicitNotFoundTest extends AnyFunSuite with CompilationErrorAssertions {
  test("simple") {
    assert(typeErrorFor("Implicits.infer[Stuff]") == "no stuff available")
  }

  test("with dependencies") {
    assert(typeErrorFor("Implicits.infer[OtherStuff]") == "no other stuff available because: no stuff available")
  }
}
