package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

class DiscardedMonixTaskTest extends AnyFunSuite with AnalyzerTest {
  test("simple") {
    assertErrors(10,
      """
        |import monix.eval.Task
        |
        |object whatever {
        |  def task: Task[String] = ???
        |
        |  // errors from these
        |  task
        |
        |  { println(""); task }
        |
        |  if(true) task else task
        |
        |  try task catch { case _: Exception => task } finally task
        |
        |  Seq(1,2,3).foreach(_ => task)
        |
        |  while(true) task
        |
        |  do task while(true)
        |
        |  // no errors from these
        |  Seq(1,2,3).map(_ => task)
        |  val tsk = task
        |}
      """.stripMargin
    )
  }
}
