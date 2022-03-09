package com.avsystem.commons
package mongo

import org.bson.types.Decimal128
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Decimal128UtilsTest extends AnyFunSuite with ScalaCheckPropertyChecks {
  test("Decimal128Utils.fromBigDecimal is equivalent to new Decimal128") {
    forAll(Arbitrary.arbitrary[BigDecimal]) { bd: BigDecimal =>
      val usingUtils = Decimal128Utils.fromBigDecimal(bd)
      val usingConstructor = try new Decimal128(bd.bigDecimal).opt catch {
        case _: NumberFormatException => Opt.Empty
      }
      assert(usingUtils == usingConstructor)
    }
  }
}
