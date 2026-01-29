package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.{GenCodec, HasGenCodec}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import java.math.MathContext

trait SerializationTestUtils {
  private def limitMathContext(bd: BigDecimal) =
    if (bd.mc == MathContext.UNLIMITED) bd(BigDecimal.defaultMathContext) else bd

  case class TestCC(i: Int, l: Long, intAsDouble: Double, b: Boolean, s: String, list: List[Char]) derives GenCodec
  case class NestedTestCC(i: Int, t: TestCC, t2: TestCC) derives GenCodec
  case class DeepNestedTestCC(n: TestCC, l: DeepNestedTestCC) derives GenCodec
  case class CompleteItem(
    unit: Unit,
    string: String,
    char: Char,
    boolean: Boolean,
    byte: Byte,
    short: Short,
    int: Int,
    long: Long,
    float: Float,
    double: Double,
    bigInt: BigInt,
    bigDecimal: BigDecimal,
    binary: Array[Byte],
    list: List[String],
    set: Set[String],
    obj: TestCC,
    map: Map[String, Int],
  ) derives GenCodec
  object TestCC {
    given Arbitrary[TestCC] = Arbitrary(for {
      i <- arbitrary[Int]
      l <- arbitrary[Long]
      b <- arbitrary[Boolean]
      s <- arbitrary[String]
      list <- arbitrary[List[Char]]
    } yield TestCC(i, l, i.toDouble, b, s, list))
  }
  object CompleteItem  {
    given Arbitrary[CompleteItem] = Arbitrary(for {
      u <- arbitrary[Unit]
      str <- arbitrary[String]
      c <- arbitrary[Char]
      bool <- arbitrary[Boolean]
      b <- arbitrary[Byte]
      s <- arbitrary[Short]
      i <- arbitrary[Int]
      l <- arbitrary[Long]
      f <- arbitrary[Float]
      d <- arbitrary[Double]
      bi <- arbitrary[BigInt]
      bd <- arbitrary[BigDecimal].map(limitMathContext)
      binary <- arbitrary[Array[Byte]]
      list <- arbitrary[List[String]]
      set <- arbitrary[Set[String]]
      obj <- arbitrary[TestCC]
      map <- arbitrary[Map[String, Int]]
    } yield CompleteItem(u, str, c, bool, b, s, i, l, f, d, bi, bd, binary, list, set, obj, map))
  }
}
