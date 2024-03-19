package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IsoInstantTest extends AnyFunSuite with ScalaCheckPropertyChecks {
  test("basic parsing") {
    assert(IsoInstant.parse("1970-01-01T00:00:00Z") == 0)
    assert(IsoInstant.parse("1970-01-01T00:00:00.000Z") == 0)
    intercept[ReadFailure](IsoInstant.parse("1970-01-01T00:00:00"))
    intercept[ReadFailure](IsoInstant.parse("1970-01-01"))
    intercept[ReadFailure](IsoInstant.parse("1970-13-01T00:00:00Z"))
    intercept[ReadFailure](IsoInstant.parse("1970-01-32T00:00:00Z"))
    intercept[ReadFailure](IsoInstant.parse("1970-01-01T25:00:00Z"))
    intercept[ReadFailure](IsoInstant.parse("1970-01-01T00:61:00Z"))
    intercept[ReadFailure](IsoInstant.parse("1970-01-01T00:00:61Z"))
  }

  test("basic formatting") {
    assert(IsoInstant.format(0) == "1970-01-01T00:00:00.000Z")
    assert(IsoInstant.format(1) == "1970-01-01T00:00:00.001Z")
  }

  test("roundtrip") {
    val genTstamp = Gen.choose[Long](-(1L << 50), 1L << 50)
    forAll(genTstamp) { (l: Long) =>
      assert(IsoInstant.parse(IsoInstant.format(l)) == l)
    }
  }
}
