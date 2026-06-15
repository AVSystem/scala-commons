package com.avsystem.commons
package misc

import com.avsystem.commons.testutil.CompilationErrorAssertions
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.implicitNotFound

@implicitNotFound("no stuff available")
sealed trait Stuff

class ImplicitNotFoundTest extends AnyFunSuite with CompilationErrorAssertions {
  test("native implicitNotFound message is used by infer") {
    assert(typeErrorFor("Implicits.infer[Stuff]") == "no stuff available")
  }
}
