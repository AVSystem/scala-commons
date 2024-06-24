package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ThrownExceptionNotInFunctionTest extends AnyFunSuite with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:-discardedMonixTask")


  test("Testing simple methods") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def ex: Exception = ???
        |
        |  def f0(x1: Int => Int) = ???
        |
        |  //not ok
        |  f0(throw ex)
        |
        |  //ok
//        |  f0(identity)
        |}""".stripMargin)
  }

  Seq(
    ("Option[_]", "map"),
    ("Option[_]", "flatMap"),
    ("List[_]", "map"),
    ("Seq[_]", "map"),
    ("Set[_]", "map"),
    ("Map[_, _]", "map"),
    ("scala.concurrent.Future[_]", "map"),
    ("scala.concurrent.Future[_]", "flatMap"),
    ("scala.util.Try[_]", "map"),
    ("Either[_, _]", "map"),
    ("monix.eval.Task[_]", "map"),
    ("monix.eval.Task[_]", "flatMap"),
    ("com.avsystem.commons.misc.Opt[_]", "map"),
    ("String => Int", "andThen"),
    ("String => Nothing", "andThen"),
    ("Nothing => Nothing", "andThen"),
    ("String => Int", "compose"),
    ("Seq[_]", "foreach"),
  ).foreach { case (tpe, function) =>
    test(s"Testing method $function of $tpe") {
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

  test("Testing multiple arguments") {
    assertErrors(26,
      //language=Scala
      """
        |object whatever {
        |  def ex: Exception = ???
        |
        |  def f1(x1: Int => Int, x2: String => String) = ???
        |  def f2(x1: Int => Int)(x2: String => String) = ???
        |  def f3(x1: Int => Int)(x2: Int)(x3: String => String) = ???
        |  def f4(x1: Int, x2: Int, x3: String => String) = ???
        |
        |  //not ok
        |  f1(throw ex, throw ex)
        |  f1(identity, throw ex)
        |  f1(throw ex, identity)
        |
        |  f2(throw ex)(throw ex)
        |  f2(identity)(throw ex)
        |  f2(throw ex)(identity)
        |
        |  f3(throw ex)(42)(throw ex)
        |  f3(throw ex)(42)(identity)
        |  f3(identity)(42)(throw ex)
        |
        |  f4(42, 42, throw ex)
        |
        |  //ok
        |  f1(identity, identity)
        |  f2(identity)(identity)
        |  f3(identity)(42)(identity)
        |  f4(42, 42, identity)
        |
        |  class A {
        |    def f1(x1: Int => Int, x2: String => String) = ???
        |    def f2(x1: Int => Int)(x2: String => String) = ???
        |    def f3(x1: Int => Int)(x2: Int)(x3: String => String) = ???
        |    def f4(x1: Int, x2: Int, x3: String => String) = ???
        |  }
        |  final val a = new A
        |
        |  //not ok
        |  a.f1(throw ex, throw ex)
        |  a.f1(identity, throw ex)
        |  a.f1(throw ex, identity)
        |
        |  a.f2(throw ex)(throw ex)
        |  a.f2(identity)(throw ex)
        |  a.f2(throw ex)(identity)
        |
        |  a.f3(throw ex)(42)(throw ex)
        |  a.f3(throw ex)(42)(identity)
        |  a.f3(identity)(42)(throw ex)
        |
        |  a.f4(42, 42, throw ex)
        |
        |  //ok
        |  a.f1(identity, identity)
        |  a.f2(identity)(identity)
        |  a.f3(identity)(42)(identity)
        |  a.f4(42, 42, identity)
        |}""".stripMargin
    )
  }

  test("Testing constructor invocation") {
    assertErrors(9,
      //language=Scala
      s"""
         |object whatever {
         |  def ex: Exception = ???
         |
         |  class A(f: Int => Int)
         |
         |  new A(throw ex)
         |
         |  class B(f: Int => Int)(g: Int => Int)
         |
         |  new B(throw ex)(identity)
         |  new B(identity)(throw ex)
         |  new B(throw ex)(throw ex)
         |
         |  class C(f: Int => Int, g: Int => Int)
         |
         |  new C(throw ex, identity)
         |  new C(identity, throw ex)
         |  new C(throw ex, throw ex)
         |}
         |""".stripMargin
    )
  }

  test("Testing indirect exception throwing") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def throwEx: Nothing = ???
        |
        |  def f0(x1: Int => Int) = ???
        |
        |  //not ok
        |  f0(throwEx)
        |
        |  //ok
        |  f0(identity)
        |}""".stripMargin)
  }
}
