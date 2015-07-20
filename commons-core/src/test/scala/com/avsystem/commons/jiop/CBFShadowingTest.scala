package com.avsystem.commons
package jiop

import org.scalatest.FunSuite

/**
 * Author: ghik
 * Created: 20/07/15.
 */
class CBFShadowingTest extends FunSuite {
  test("java collections CanBuildFroms should not shadow Scala default ones when imported directly") {
    import JavaInterop.jArrayListCBF
    jArrayListCBF // prevent IntelliJ from removing import

    assert(List(1, 2, 3).map(_.toString) === List("1", "2", "3"))
  }

  test("java collections CanBuildFroms should not shadow Scala default ones when imported by wildcard") {
    import JavaInterop._
    jArrayListCBF // prevent IntelliJ from removing import

    assert(List(1, 2, 3).map(_.toString) === List("1", "2", "3"))
  }

  test("java collections CanBuildFroms should not shadow Scala default ones when declared explicitly") {
    implicit val listCbf = JavaInterop.jArrayListCBF[String]

    assert(List(1, 2, 3).map(_.toString) === List("1", "2", "3"))
  }
}
