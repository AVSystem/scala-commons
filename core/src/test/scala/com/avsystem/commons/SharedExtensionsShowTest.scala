package com.avsystem.commons

import com.avsystem.commons.SharedExtensions.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SharedExtensionsShowTest extends AnyFunSuite with Matchers {

  test("showAst returns receiver unchanged") {
    42.showAst shouldEqual 42
  }

  test("showRawAst returns receiver unchanged") {
    "hello".showRawAst shouldEqual "hello"
  }

  test("showSymbol returns receiver unchanged") {
    42.showSymbol shouldEqual 42
  }

  test("showSymbolFullName returns receiver unchanged") {
    42.showSymbolFullName shouldEqual 42
  }

  test("showType returns receiver unchanged") {
    "x".showType shouldEqual "x"
  }

  test("showRawType returns receiver unchanged") {
    42.showRawType shouldEqual 42
  }

  test("showTypeSymbol returns receiver unchanged") {
    42.showTypeSymbol shouldEqual 42
  }

  test("showTypeSymbolFullName returns receiver unchanged") {
    42.showTypeSymbolFullName shouldEqual 42
  }

  test("sourceCode returns literal source text of the call") {
    // Scala 3 deviation from Scala 2: because UniversalOps is a wrapper class,
    // the macro's receiver Expr is the synthetic `a` constructor val (no source pos);
    // we fall back to Position.ofMacroExpansion which captures the full call site.
    val src = (1 + 2).sourceCode
    src should include("1 + 2")
  }

  test("withSourceCode returns (value, source)") {
    val (value, src) = (1 + 2).withSourceCode
    value shouldEqual 3
    src should include("1 + 2")
  }
}
