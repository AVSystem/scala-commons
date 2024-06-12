package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ThrownExceptionNotInMonixScopeTest extends AnyFunSuite with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:-discardedMonixTask")

  test("simple") {
    assertErrors(10,
      //language=Scala
      """
        |import monix.eval.Task
        |
        |object whatever {
        |  def task: Task[String] = ???
        |  def ex: Exception = ???
        |
        |  // errors from these
        |  task.map(throw ex)
        |
        |  {
        |    println(""); task.map(throw ex)
        |  }
        |
        |  if (true) task.map(throw ex) else task.map(throw ex)
        |
        |  try task.map(throw ex) catch {
        |    case _: Exception => task.map(throw ex)
        |  } finally task.map(throw ex)
        |
        |  Seq(1, 2, 3).foreach(_ => task.map(throw ex))
        |
        |  while (true) task.map(throw ex)
        |
        |  do task.map(throw ex) while (true)
        |
        |  // no errors from these
        |  task.map(_ => throw ex)
        |
        |  {
        |    println(""); task.map(_ => throw ex)
        |  }
        |
        |  if (true) task.map(_ => throw ex) else task.map(_ => throw ex)
        |
        |  try task.map(_ => throw ex) catch {
        |    case _: Exception => task.map(_ => throw ex)
        |  } finally task.map(_ => throw ex)
        |
        |  Seq(1, 2, 3).foreach(_ => task.map(_ => throw ex))
        |
        |  while (true) task.map(_ => throw ex)
        |
        |  do task.map(_ => throw ex) while (true)
        |}
        |
        |""".stripMargin
    )
  }
}
