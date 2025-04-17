package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

class ImplicitFunctionParamsTest extends AnyFunSuite with AnalyzerTest {
  test("implicit parameters should not be function types or partial functions") {
    assertErrors(8,
      //language=Scala
      """
        |object whatever {
        |  // This should pass - regular parameter with function type
        |  def goodMethod1(f: Int => String): Unit = ???
        |
        |  // This should pass - regular parameter with partial function type
        |  def goodMethod2(pf: PartialFunction[Int, String]): Unit = ???
        |
        |  // This should pass - implicit parameter with non-function type
        |  def goodMethod3(implicit s: String): Unit = ???
        |
        |  // This should fail - implicit parameter with function type
        |  def badMethod1(implicit f: Int => String): Unit = ???
        |
        |  // This should fail - implicit parameter with function type in second parameter list
        |  def badMethod2(x: Int)(implicit f: Int => String): Unit = ???
        |
        |  // This should fail - implicit parameter with partial function type
        |  def badMethod3(implicit pf: PartialFunction[Int, String]): Unit = ???
        |
        |  // This should fail - implicit parameter with partial function type in second parameter list
        |  def badMethod4(x: Int)(implicit pf: PartialFunction[Int, String]): Unit = ???
        |
        |  // This should pass - regular class parameter with function type
        |  class GoodClass1(f: Int => String)
        |
        |  // This should pass - regular class parameter with partial function type
        |  class GoodClass2(pf: PartialFunction[Int, String])
        |
        |  // This should pass - implicit class parameter with non-function type
        |  class GoodClass3(implicit s: String)
        |
        |  // This should fail - implicit class parameter with function type
        |  class BadClass1(implicit f: Int => String)
        |
        |  // This should fail - implicit class parameter with function type in second parameter list
        |  class BadClass2(x: Int)(implicit f: Int => String)
        |
        |  // This should fail - implicit class parameter with partial function type
        |  class BadClass3(implicit pf: PartialFunction[Int, String])
        |
        |  // This should fail - implicit class parameter with partial function type in second parameter list
        |  class BadClass4(x: Int)(implicit pf: PartialFunction[Int, String])
        |}
      """.stripMargin)
  }
}