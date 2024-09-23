package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.{SimpleValueInput, SimpleValueOutput}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WrappedJsonTest extends AnyFunSuite with Matchers {

  private val testJson = """{"a": 123, "b": 3.14}"""

  test("WrappedJson with JSON input/output") {
    assert(JsonStringOutput.write(WrappedJson(testJson)) == testJson)
    assert(JsonStringInput.read[WrappedJson](testJson) == WrappedJson(testJson))
  }

  // SimpleValueInput/Output does not support RawJson marker
  test("WrappedJson with plain input/output") {
    assert(SimpleValueOutput.write(WrappedJson(testJson)) == testJson)
    assert(SimpleValueInput.read[WrappedJson](testJson) == WrappedJson(testJson))
  }
}
