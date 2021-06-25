package com.avsystem.commons
package di

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class ComponentsTest extends AnyFunSuite {
  object cycle extends Components {
    val a: Component[String] = component(b.ref.toUpperCase)
    val b: Component[String] = component(c.ref.toLowerCase)
    val c: Component[String] = component(a.ref.trim)
  }

  test("cycle detection test") {
    import ExecutionContext.Implicits.global
    assertThrows[DependencyCycleException](
      Await.result(cycle.a.init, Duration.Inf)
    )
  }
}
