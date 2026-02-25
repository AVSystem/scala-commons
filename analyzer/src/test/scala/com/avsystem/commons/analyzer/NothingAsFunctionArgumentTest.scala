package com.avsystem.commons
package analyzer

import org.scalatest.wordspec.AnyWordSpec

final class NothingAsFunctionArgumentTest extends AnyWordSpec with AnalyzerTest {
  settings.pluginOptions.value ++= List("AVSystemAnalyzer:-discardedMonixTask")

  "The ThrownExceptionNotInFunction rule" should {
    "detect improper usage of thrown exceptions" when {

      "an exception is thrown directly in a simple function argument" in assertErrors(
        1,
        scala"""
               |def ex: Exception = ???
               |
               |def f0(x1: Int => Int): Unit = ???
               |
               |// This is incorrect: throwing an exception instead of passing a function
               |f0(throw ex)
               |
               |// This is correct: passing a valid function
               |f0(identity)
               |""".stripMargin,
      )

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
        ("String => Int", "compose[Any]"),
        ("Seq[_]", "foreach"),
      ).foreach { case (typeName, methodName) =>
        val definition =
          // language=Scala
          s"""
             |def sth: $typeName = ???
             |def ex: Exception = ???
             |
             |implicit val ec: scala.concurrent.ExecutionContext = ??? // Required for Future
             |""".stripMargin

        s"an exception is thrown directly in the $methodName method of $typeName".which {

          "occurs within the method body" in assertErrors(
            1,
            scala"""
                   |$definition
                   |sth.$methodName(throw ex)
                   |""".stripMargin,
          )

          "occurs within a code block" in assertErrors(
            1,
            scala"""
                   |$definition
                   |{
                   |  println(""); sth.$methodName(throw ex)
                   |}
                   |""".stripMargin,
          )

          "occurs within both branches of an if expression" in assertErrors(
            2,
            scala"""
                   |$definition
                   |if (true) sth.$methodName(throw ex) else sth.$methodName(throw ex)
                   |""".stripMargin,
          )

          "occurs within all sections of a try-catch-finally block" in assertErrors(
            3,
            scala"""
                   |$definition
                   |try sth.$methodName(throw ex) catch {
                   |  case _: Exception => sth.$methodName(throw ex)
                   |} finally sth.$methodName(throw ex)
                   |""".stripMargin,
          )

          "occurs within a while loop" in assertErrors(
            1,
            scala"""
                   |$definition
                   |while (true) sth.$methodName(throw ex)
                   |""".stripMargin,
          )

          "occurs within a do-while loop" in assertErrors(
            1,
            scala"""
                   |$definition
                   |do sth.$methodName(throw ex) while (true)
                   |""".stripMargin,
          )

          "occurs within a function invocation" in assertErrors(
            1,
            scala"""
                   |$definition
                   |Seq(1, 2, 3).foreach(_ => sth.$methodName(throw ex))
                   |""".stripMargin,
          )

          "does not occur when the exception is thrown inside a function literal" in assertNoErrors(
            scala"""
                   |$definition
                   |
                   |sth.$methodName(_ => throw ex)
                   |
                   |{
                   |  println(""); sth.$methodName(_ => throw ex)
                   |}
                   |
                   |if (true) sth.$methodName(_ => throw ex) else sth.$methodName(_ => throw ex)
                   |
                   |try sth.$methodName(_ => throw ex) catch {
                   |  case _: Exception => sth.$methodName(_ => throw ex)
                   |} finally sth.$methodName(_ => throw ex)
                   |
                   |Seq(1, 2, 3).foreach(_ => sth.$methodName(_ => throw ex))
                   |
                   |while (true) sth.$methodName(_ => throw ex)
                   |
                   |do sth.$methodName(_ => throw ex) while (true)
                   |""".stripMargin
          )
        }
      }

      "multiple-parameter functions are tested for thrown exceptions" in {
        val definition =
          // language=Scala
          """
            |def ex: Exception = ???
            |
            |def f1(x1: Int => Int, x2: String => String): Unit = ???
            |def f2(x1: Int => Int)(x2: String => String): Unit = ???
            |def f3(x1: Int => Int)(x2: Int)(x3: String => String): Unit = ???
            |def f4(x1: Int, x2: Int, x3: String => String): Unit = ???
            |""".stripMargin

        assertErrors(
          13,
          scala"""
                 |$definition
                 |
                 |f1(throw ex, throw ex)
                 |f1(identity, throw ex)
                 |f1(throw ex, identity)
                 |
                 |f2(throw ex)(throw ex)
                 |f2(identity)(throw ex)
                 |f2(throw ex)(identity)
                 |
                 |f3(throw ex)(42)(throw ex)
                 |f3(throw ex)(42)(identity)
                 |f3(identity)(42)(throw ex)
                 |
                 |f4(42, 42, throw ex)
                 |""".stripMargin,
        )

        assertNoErrors(
          scala"""$definition
                 |
                 |f1(identity, identity)
                 |f2(identity)(identity)
                 |f3(identity)(42)(identity)
                 |f4(42, 42, identity)
                 |""".stripMargin
        )
      }

      "methods with parameters in a class context are tested" in {
        val definition =
          // language=Scala
          """
            |class A {
            |  def f1(x1: Int => Int, x2: String => String): Unit = ???
            |  def f2(x1: Int => Int)(x2: String => String): Unit = ???
            |  def f3(x1: Int => Int)(x2: Int)(x3: String => String): Unit = ???
            |  def f4(x1: Int, x2: Int, x3: String => String): Unit = ???
            |}
            |final val a = new A
            |""".stripMargin

        assertErrors(
          15,
          scala"""
                 |$definition
                 |
                 |a.f1(throw ex, throw ex)
                 |a.f1(identity, throw ex)
                 |a.f1(throw ex, identity)
                 |
                 |a.f2(throw ex)(throw ex)
                 |a.f2(identity)(throw ex)
                 |a.f2(throw ex)(identity)
                 |
                 |a.f3(throw ex)(42)(throw ex)
                 |a.f3(throw ex)(42)(identity)
                 |a.f3(identity)(42)(throw ex)
                 |a.f4(42, 42, throw ex)
                 |
                 |""".stripMargin,
        )

        assertNoErrors(
          scala"""
                 |$definition
                 |
                 |a.f1(identity, identity)
                 |a.f2(identity)(identity)
                 |a.f3(identity)(42)(identity)
                 |a.f4(42, 42, identity)
                 |""".stripMargin
        )
      }

      "an exception is thrown directly in a class constructor parameter" in {
        assertErrors(
          9,
          scala"""
                 |def ex: Exception = ???
                 |
                 |class A(f: Int => Int)
                 |new A(throw ex)
                 |
                 |class B(f: Int => Int)(g: Int => Int)
                 |new B(throw ex)(identity)
                 |new B(identity)(throw ex)
                 |new B(throw ex)(throw ex)
                 |
                 |class C(f: Int => Int, g: Int => Int)
                 |new C(throw ex, identity)
                 |new C(identity, throw ex)
                 |new C(throw ex, throw ex)
                 |""".stripMargin,
        )
      }

      "detects passing a function that throws an exception (Nothing)" in {
        assertErrors(
          1,
          scala"""
                 |def throwEx: Nothing = ???
                 |
                 |def f0(x1: Int => Int): Unit = ???
                 |
                 |// Incorrect usage: passing a thrown exception instead of a function
                 |f0(throwEx)
                 |
                 |// Correct usage: passing a valid function
                 |f0(identity)
                 |""".stripMargin,
        )
      }
    }
  }
}
