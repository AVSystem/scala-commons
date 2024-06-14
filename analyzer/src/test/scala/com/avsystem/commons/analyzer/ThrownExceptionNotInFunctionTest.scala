package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ThrownExceptionNotInFunctionTest extends AnyFunSuite with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:-discardedMonixTask")

  Seq(
    ("Option[_]", "map"),
    ("List[_]", "map"),
    ("Seq[_]", "map"),
    ("Set[_]", "map"),
    ("Map[_, _]", "map"),
    ("scala.concurrent.Future[_]", "map"),
    ("scala.util.Try[_]", "map"),
    ("Either[_, _]", "map"),
    ("monix.eval.Task[_]", "map"),
    ("com.avsystem.commons.misc.Opt[_]", "map"),
    ("String => Int", "andThen"),
    ("String => Nothing", "andThen"),
    ("Nothing => Nothing", "andThen"),
    ("String => Int", "compose"),
    ("Seq[_]", "foreach"),
  ).foreach { case (tpe, function) =>
    test(s"Testing $function of $tpe") {
      assertErrors(10,
        //language=Scala
        s"""
           |object whatever {
           |  implicit val ec: scala.concurrent.ExecutionContext = ??? // for Future
           |
           |  def sth: $tpe = ???
           |  def ex: Exception = ???
           |
           |  // errors from these
           |  sth.$function(throw ex)
           |
           |  {
           |    println(""); sth.$function(throw ex)
           |  }
           |
           |  if (true) sth.$function(throw ex) else sth.$function(throw ex)
           |
           |  try sth.$function(throw ex) catch {
           |    case _: Exception => sth.$function(throw ex)
           |  } finally sth.$function(throw ex)
           |
           |  Seq(1, 2, 3).foreach(_ => sth.$function(throw ex))
           |
           |  while (true) sth.$function(throw ex)
           |
           |  do sth.$function(throw ex) while (true)
           |
           |  // no errors from these
           |  sth.$function(_ => throw ex)
           |
           |  {
           |    println(""); sth.$function(_ => throw ex)
           |  }
           |
           |  if (true) sth.$function(_ => throw ex) else sth.$function(_ => throw ex)
           |
           |  try sth.$function(_ => throw ex) catch {
           |    case _: Exception => sth.$function(_ => throw ex)
           |  } finally sth.$function(_ => throw ex)
           |
           |  Seq(1, 2, 3).foreach(_ => sth.$function(_ => throw ex))
           |
           |  while (true) sth.$function(_ => throw ex)
           |
           |  do sth.$function(_ => throw ex) while (true)
           |}
           |
           |""".stripMargin
      )
    }
  }
}

