package com.avsystem.commons
package mongo

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class BigDecimalEncodingTest extends FunSuite with PropertyChecks {
  test("BigDecimal BSON encoding") {
    forAll { value: BigDecimal =>
      assert(value == BsonInput.bigDecimalFromBytes(BsonOutput.bigDecimalBytes(value)))
    }
  }
}
