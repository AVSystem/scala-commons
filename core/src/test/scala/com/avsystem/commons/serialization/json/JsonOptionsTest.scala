package com.avsystem.commons
package serialization.json

import org.scalatest.funsuite.AnyFunSuite

class JsonOptionsTest extends AnyFunSuite {
  private val opts = JsonOptions()

  test("default integerReading") {
    assert(JsonOptions().integerReading == JsonIntegerReading.Default)
    assert(JsonOptions.Default.integerReading == JsonIntegerReading.Default)
  }

  test("named-argument copy keeps other fields") {
    val exact = JsonOptions(asciiOutput = true).copy(integerReading = JsonIntegerReading.Exact)
    assert(exact.asciiOutput)
    assert(exact.integerReading == JsonIntegerReading.Exact)
    val pretty = exact.copy(formatting = JsonFormatting.Pretty)
    assert(pretty.integerReading == JsonIntegerReading.Exact)
    assert(pretty.asciiOutput)
  }

  test("5-argument apply") {
    val o = JsonOptions(opts.formatting, opts.asciiOutput, opts.mathContext, opts.dateFormat, opts.binaryFormat)
    assert(o.integerReading == JsonIntegerReading.Default)
    assert(o == JsonOptions.Default)
  }

  test("5-argument copy keeps integerReading") {
    val exact = opts.copy(integerReading = JsonIntegerReading.Exact)
    val o = exact.copy(opts.formatting, true, opts.mathContext, opts.dateFormat, opts.binaryFormat)
    assert(o.integerReading == JsonIntegerReading.Exact)
    assert(o.asciiOutput)
  }

  test("5-argument constructor") {
    val o = new JsonOptions(opts.formatting, opts.asciiOutput, opts.mathContext, opts.dateFormat, opts.binaryFormat)
    assert(o.integerReading == JsonIntegerReading.Default)
  }

  test("6-field extractor") {
    val JsonOptions(_, _, _, _, _, ir) = opts.copy(integerReading = JsonIntegerReading.Exact)
    assert(ir == JsonIntegerReading.Exact)
  }
}
