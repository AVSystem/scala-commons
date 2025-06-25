package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitFunctionParamsTest extends AnyFunSuite with AnalyzerTest {
  test("regular parameter with function type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  def goodMethod1(f: Int => String): Unit = ???
        |}
      """.stripMargin)
  }

  test("regular parameter with partial function type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  def goodMethod2(pf: PartialFunction[Int, String]): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with non-function type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  def goodMethod3(implicit s: String): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with function type should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def badMethod1(implicit f: Int => String): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with function type in second parameter list should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def badMethod2(x: Int)(implicit f: Int => String): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with partial function type should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def badMethod3(implicit pf: PartialFunction[Int, String]): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with partial function type in second parameter list should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def badMethod4(x: Int)(implicit pf: PartialFunction[Int, String]): Unit = ???
        |}
      """.stripMargin)
  }

  test("regular class parameter with function type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  class GoodClass1(f: Int => String)
        |}
      """.stripMargin)
  }

  test("regular class parameter with partial function type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  class GoodClass2(pf: PartialFunction[Int, String])
        |}
      """.stripMargin)
  }

  test("implicit class parameter with non-function type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  class GoodClass3(implicit s: String)
        |}
      """.stripMargin)
  }

  test("implicit class parameter with function type should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  class BadClass1(implicit f: Int => String)
        |}
      """.stripMargin)
  }

  test("implicit class parameter with function type in second parameter list should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  class BadClass2(x: Int)(implicit f: Int => String)
        |}
      """.stripMargin)
  }

  test("implicit class parameter with partial function type should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  class BadClass3(implicit pf: PartialFunction[Int, String])
        |}
      """.stripMargin)
  }

  test("implicit class parameter with partial function type in second parameter list should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  class BadClass4(x: Int)(implicit pf: PartialFunction[Int, String])
        |}
      """.stripMargin)
  }

  // Define a SAM type (Single Abstract Method) for all SAM type tests
  trait IntToString {
    def apply(i: Int): String
  }

  test("regular parameter with SAM type should pass") {
    assertNoErrors(
      //language=Scala
        """
          |object whatever {
          |  def goodMethod1(f: IntToString): Unit = ???
          |}
      """.stripMargin)
  }

  test("implicit parameter with SAM type should pass") {
    assertNoErrors(
      //language=Scala
        """
          |object whatever {
          |  def goodMethod2(implicit f: IntToString): Unit = ???
          |}
      """.stripMargin)
  }

  test("implicit parameter with SAM type in second parameter list should pass") {
    assertNoErrors(
      //language=Scala
        """
          |object whatever {
          |  def goodMethod3(x: Int)(implicit f: IntToString): Unit = ???
          |}
      """.stripMargin)
  }

  test("regular class parameter with SAM type should pass") {
    assertNoErrors(
      //language=Scala
        """
          |object whatever {
          |  class GoodClass1(f: IntToString)
          |}
      """.stripMargin)
  }

  test("implicit class parameter with SAM type should pass") {
    assertNoErrors(
      //language=Scala
        """
          |object whatever {
          |  class GoodClass2(implicit f: IntToString)
          |}
      """.stripMargin)
  }

  test("implicit class parameter with SAM type in second parameter list should pass") {
    assertNoErrors(
      //language=Scala
        """
          |object whatever {
          |  class GoodClass3(x: Int)(implicit f: IntToString)
          |}
      """.stripMargin)
  }

  test("regular parameter with Function2 type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  def goodMethod1(f: (Int, String) => Boolean): Unit = ???
        |}
      """.stripMargin)
  }

  test("regular parameter with Function3 type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  def goodMethod2(f: (Int, String, Double) => Boolean): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with non-function type should pass (multiple params context)") {
    assertNoErrors(
      //language=Scala
      """
        |object whatever {
        |  def goodMethod3(implicit s: String): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with Function2 type should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def badMethod1(implicit f: (Int, String) => Boolean): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with Function2 type in second parameter list should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def badMethod2(x: Int)(implicit f: (Int, String) => Boolean): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with Function3 type should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def badMethod3(implicit f: (Int, String, Double) => Boolean): Unit = ???
        |}
      """.stripMargin)
  }

  test("implicit parameter with Function3 type in second parameter list should fail") {
    assertErrors(1,
      //language=Scala
      """
        |object whatever {
        |  def badMethod4(x: Int)(implicit f: (Int, String, Double) => Boolean): Unit = ???
        |}
      """.stripMargin)
  }

  test("regular class parameter with Function2 type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |class GoodClass1(f: (Int, String) => Boolean)
      """.stripMargin)
  }

  test("regular class parameter with Function3 type should pass") {
    assertNoErrors(
      //language=Scala
      """
        |class GoodClass2(f: (Int, String, Double) => Boolean)
      """.stripMargin)
  }

  test("implicit class parameter with non-function type should pass (multiple params context)") {
    assertNoErrors(
      //language=Scala
      """
        |class GoodClass3(implicit s: String)
      """.stripMargin)
  }

  test("implicit class parameter with Function2 type should fail") {
    assertErrors(1,
      //language=Scala
      """
        |class BadClass1(implicit f: (Int, String) => Boolean)
      """.stripMargin)
  }

  test("implicit class parameter with Function2 type in second parameter list should fail") {
    assertErrors(1,
      //language=Scala
      """
        |class BadClass2(x: Int)(implicit f: (Int, String) => Boolean)
      """.stripMargin)
  }

  test("implicit class parameter with Function3 type should fail") {
    assertErrors(1,
      //language=Scala
      """
        |class BadClass3(implicit f: (Int, String, Double) => Boolean)
      """.stripMargin)
  }

  test("implicit class parameter with Function3 type in second parameter list should fail") {
    assertErrors(1,
      //language=Scala
      """
        |class BadClass4(x: Int)(implicit f: (Int, String, Double) => Boolean)
      """.stripMargin)
  }
}
