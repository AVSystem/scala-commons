package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class DiscardedMonixTaskTest extends AnyFunSuite with AnalyzerTest {

  // Only enable the discardedMonixTask rule to avoid interference from other rules
  override protected def pluginOptions: List[String] = List("AVSystemAnalyzer:+discardedMonixTask")

  test("discarded Task in block should be rejected") {
    assertErrors(
      1,
      scala"""
             |import monix.eval.Task
             |def test(): Unit = {
             |  Task.eval(42)
             |  ()
             |}
             |""".stripMargin,
    )
  }

  test("assigned Task should not be rejected") {
    assertNoErrors(
      scala"""
             |import monix.eval.Task
             |val t: Task[Int] = Task.eval(42)
             |""".stripMargin,
    )
  }

  test("returned Task should not be rejected") {
    assertNoErrors(
      scala"""
             |import monix.eval.Task
             |def test(): Task[Int] = Task.eval(42)
             |""".stripMargin,
    )
  }

  test("discarded Task in while loop body should be rejected") {
    assertErrors(
      1,
      scala"""
             |import monix.eval.Task
             |def test(): Unit = {
             |  var i = 0
             |  while (i < 10) {
             |    Task.eval(i)
             |    i += 1
             |  }
             |}
             |""".stripMargin,
    )
  }

  test("Task in if/else non-discarded position should not be rejected") {
    assertNoErrors(
      scala"""
             |import monix.eval.Task
             |def test(): Task[Int] = if (true) Task.eval(1) else Task.now(2)
             |""".stripMargin,
    )
  }

  test("multiple discarded Tasks should all be rejected") {
    assertErrors(
      2,
      scala"""
             |import monix.eval.Task
             |def test(): Unit = {
             |  Task.eval(1)
             |  Task.eval(2)
             |  ()
             |}
             |""".stripMargin,
    )
  }
}
