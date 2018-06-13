package com.avsystem.commons
package mongo

import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class BigDecimalEncodingTest extends FunSuite with PropertyChecks {
  test("BigDecimal BSON encoding") {
    forAll(Arbitrary.arbitrary[BigDecimal].map(_.bigDecimal)) { value =>
      assert(value == BsonInput.bigDecimalFromBytes(BsonOutput.bigDecimalBytes(value)))
    }
  }
}
