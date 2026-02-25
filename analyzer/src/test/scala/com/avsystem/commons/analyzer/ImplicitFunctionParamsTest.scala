package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitFunctionParamsTest extends AnyFunSuite with AnalyzerTest {
  test("regular parameter with function type should pass") {
    assertNoErrors(scala"def goodMethod1(f: Int => String): Unit = ???")
  }

  test("regular parameter with partial function type should pass") {
    assertNoErrors(scala"def goodMethod2(pf: PartialFunction[Int, String]): Unit = ???")
  }

  test("implicit parameter with non-function type should pass") {
    assertNoErrors(scala"def goodMethod3(implicit s: String): Unit = ???")
  }

  test("implicit parameter with function type should fail") {
    assertErrors(1, scala"def badMethod1(implicit f: Int => String): Unit = ???")
  }

  test("implicit parameter with function type in second parameter list should fail") {
    assertErrors(1, scala"def badMethod2(x: Int)(implicit f: Int => String): Unit = ???")
  }

  test("implicit parameter with partial function type should fail") {
    assertErrors(1, scala"def badMethod3(implicit pf: PartialFunction[Int, String]): Unit = ???")
  }

  test("implicit parameter with partial function type in second parameter list should fail") {
    assertErrors(1, scala"def badMethod4(x: Int)(implicit pf: PartialFunction[Int, String]): Unit = ???")
  }

  test("regular class parameter with function type should pass") {
    assertNoErrors(scala"class GoodClass1(f: Int => String)")
  }

  test("regular class parameter with partial function type should pass") {
    assertNoErrors(scala"class GoodClass2(pf: PartialFunction[Int, String])")
  }

  test("implicit class parameter with non-function type should pass") {
    assertNoErrors(scala"class GoodClass3(implicit s: String)")
  }

  test("implicit class parameter with function type should fail") {
    assertErrors(1, scala"class BadClass1(implicit f: Int => String)")
  }

  test("implicit class parameter with function type in second parameter list should fail") {
    assertErrors(1, scala"class BadClass2(x: Int)(implicit f: Int => String)")
  }

  test("implicit class parameter with partial function type should fail") {
    assertErrors(1, scala"class BadClass3(implicit pf: PartialFunction[Int, String])")
  }

  test("implicit class parameter with partial function type in second parameter list should fail") {
    assertErrors(1, scala"class BadClass4(x: Int)(implicit pf: PartialFunction[Int, String])")
  }

  // Define a SAM type (Single Abstract Method) for all SAM type tests
  private val SamDefinition =
    // language=Scala
    """
      |trait IntToString {
      |    def apply(i: Int): String
      |}
      |""".stripMargin

  test("regular parameter with SAM type should pass") {
    assertNoErrors(scala"$SamDefinition def goodMethod1(f: IntToString): Unit = ???")
  }

  test("implicit parameter with SAM type should pass") {
    assertNoErrors(scala"$SamDefinition def goodMethod2(implicit f: IntToString): Unit = ???")
  }

  test("implicit parameter with SAM type in second parameter list should pass") {
    assertNoErrors(scala"$SamDefinition def goodMethod3(x: Int)(implicit f: IntToString): Unit = ???")
  }

  test("regular class parameter with SAM type should pass") {
    assertNoErrors(scala"$SamDefinition class GoodClass1(f: IntToString)")
  }

  test("implicit class parameter with SAM type should pass") {
    assertNoErrors(scala"$SamDefinition class GoodClass2(implicit f: IntToString)")
  }

  test("implicit class parameter with SAM type in second parameter list should pass") {
    assertNoErrors(scala"$SamDefinition class GoodClass3(x: Int)(implicit f: IntToString)")
  }

  test("regular parameter with Function2 type should pass") {
    assertNoErrors(scala"def goodMethod1(f: (Int, String) => Boolean): Unit = ???")
  }

  test("regular parameter with Function3 type should pass") {
    assertNoErrors(scala"def goodMethod2(f: (Int, String, Double) => Boolean): Unit = ???")
  }

  test("implicit parameter with non-function type should pass (multiple params context)") {
    assertNoErrors(scala"def goodMethod3(implicit s: String): Unit = ???")
  }

  test("implicit parameter with Function2 type should fail") {
    assertErrors(1, scala"def badMethod1(implicit f: (Int, String) => Boolean): Unit = ???")
  }

  test("implicit parameter with Function2 type in second parameter list should fail") {
    assertErrors(1, scala"def badMethod2(x: Int)(implicit f: (Int, String) => Boolean): Unit = ???")
  }

  test("implicit parameter with Function3 type should fail") {
    assertErrors(1, scala"def badMethod3(implicit f: (Int, String, Double) => Boolean): Unit = ???")
  }

  test("implicit parameter with Function3 type in second parameter list should fail") {
    assertErrors(1, scala"def badMethod4(x: Int)(implicit f: (Int, String, Double) => Boolean): Unit = ???")
  }

  test("regular class parameter with Function2 type should pass") {
    assertNoErrors(scala"class GoodClass1(f: (Int, String) => Boolean)")
  }

  test("regular class parameter with Function3 type should pass") {
    assertNoErrors(scala"class GoodClass2(f: (Int, String, Double) => Boolean)")
  }

  test("implicit class parameter with non-function type should pass (multiple params context)") {
    assertNoErrors(scala"class GoodClass3(implicit s: String)")
  }

  test("implicit class parameter with Function2 type should fail") {
    assertErrors(1, scala"class BadClass1(implicit f: (Int, String) => Boolean)")
  }

  test("implicit class parameter with Function2 type in second parameter list should fail") {
    assertErrors(1, scala"class BadClass2(x: Int)(implicit f: (Int, String) => Boolean)")
  }

  test("implicit class parameter with Function3 type should fail") {
    assertErrors(1, scala"class BadClass3(implicit f: (Int, String, Double) => Boolean)")
  }

  test("implicit class parameter with Function3 type in second parameter list should fail") {
    assertErrors(1, scala"class BadClass4(x: Int)(implicit f: (Int, String, Double) => Boolean)")
  }
}
