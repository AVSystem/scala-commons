package com.avsystem.commons
package mongo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BigDecimalEncodingTest extends AnyFunSuite with ScalaCheckPropertyChecks {
  test("BigDecimal BSON encoding") {
    forAll { (value: BigDecimal) =>
      assert(value == BsonInput.bigDecimalFromBytes(BsonOutput.bigDecimalBytes(value)))
    }
  }
}
