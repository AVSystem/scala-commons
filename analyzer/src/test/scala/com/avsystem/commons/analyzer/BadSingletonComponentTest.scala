package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class BadSingletonComponentTest extends AnyFunSuite with AnalyzerTest {
  test("general") {
    assertErrors(
      5,
      scala"""
             |import com.avsystem.commons.di._
             |
             |object test extends Components {
             |  singleton(123)
             |  val notDef = singleton(123)
             |  def hasParams(param: Int) = singleton(param)
             |  def hasTypeParams[T]: Component[T] = singleton(???)
             |  def outerMethod: Component[Int] = {
             |    def innerMethod = singleton(123)
             |    innerMethod
             |  }
             |
             |  def good: Component[Int] = singleton(123)
             |  def alsoGood: Component[Int] = { singleton(123) }
             |  def goodAsWell: Component[Int] = singleton(123).dependsOn(good)
             |}
             |""".stripMargin,
    )
  }

  test("extension methods reached via implicit conversion are accepted") {
    // Without the Unwrap case for single-argument adapter Applys, the implicit conversion
    // `componentExt(...)` would interrupt the prefix-type check (its type is the wrapper class,
    // not `Component[T]`), causing a false positive on what is in fact a shape-preserving chain.
    assertNoErrors(
      scala"""
             |import com.avsystem.commons.di._
             |
             |object test extends Components {
             |  class ComponentExt[T](c: Component[T]) {
             |    def chained(f: T => Unit): Component[T] = c
             |  }
             |  implicit def componentExt[T](c: Component[T]): ComponentExt[T] = new ComponentExt(c)
             |
             |  def good: Component[Int] = singleton(123)
             |  def withExt: Component[Int] = singleton(123).chained(_ => ())
             |  // also works when mixed with native shape-preserving methods on Component
             |  def withExtAndNative: Component[Int] = singleton(123).chained(_ => ()).dependsOn(good)
             |  def withNativeAndExt: Component[Int] = singleton(123).dependsOn(good).chained(_ => ())
             |}
             |""".stripMargin
    )
  }
}
