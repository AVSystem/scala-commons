package com.avsystem.commons
package mongo

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class BigDecimalEncodingTest extends AnyFunSuite with ScalaCheckPropertyChecks {
  test("BigDecimal BSON encoding") {
    forAll { value: BigDecimal =>
      assert(value == BsonInput.bigDecimalFromBytes(BsonOutput.bigDecimalBytes(value)))
    }
  }
}
