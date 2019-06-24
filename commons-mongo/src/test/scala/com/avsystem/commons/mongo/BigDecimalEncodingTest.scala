package com.avsystem.commons
package mongo

import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BigDecimalEncodingTest extends FunSuite with ScalaCheckPropertyChecks {
  test("BigDecimal BSON encoding") {
    forAll { value: BigDecimal =>
      assert(value == BsonInput.bigDecimalFromBytes(BsonOutput.bigDecimalBytes(value)))
    }
  }
}
