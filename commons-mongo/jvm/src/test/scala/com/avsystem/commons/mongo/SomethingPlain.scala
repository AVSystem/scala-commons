package com.avsystem.commons
package mongo

import com.avsystem.commons.misc.Bytes
import com.avsystem.commons.serialization.HasGenCodec
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

case class SomethingPlain(
  string: String,
  boolean: Boolean,
  int: Int,
  long: Long,
  timestamp: JDate,
  double: Double,
  binary: Bytes,
  list: List[String],
  map: Map[String, String]
)
object SomethingPlain extends HasGenCodec[SomethingPlain] {
  def sizedListOf[T](maxSize: Int, gen: => Gen[T]): Gen[List[T]] = {
    Gen.resize(maxSize, Gen.listOf(gen))
  }

  val stringListGen: Gen[List[String]] = sizedListOf(8, arbitrary[String])

  val entryGen: Gen[(String, String)] = for {
    key <- Gen.alphaStr
    value <- arbitrary[String]
  } yield key -> value

  val gen: Gen[SomethingPlain] = for {
    string <- arbitrary[String]
    boolean <- arbitrary[Boolean]
    int <- arbitrary[Int]
    long <- Gen.oneOf(
      Gen.choose(Long.MinValue, Int.MinValue - 1L),
      Gen.choose(Int.MaxValue + 1L, Long.MaxValue),
    )
    timestamp <- arbitrary[JDate]
    double <- arbitrary[Double]
    binary <- Gen.buildableOf[Array[Byte], Byte](arbitrary[Byte]).map(new Bytes(_))
    list <- stringListGen
    map <- Gen.mapOf(entryGen)
  } yield SomethingPlain(
    string,
    boolean,
    int,
    long,
    timestamp,
    double,
    binary,
    list,
    map
  )
}

case class SomethingComplex(
  embeddedObject: SomethingPlain,
  complexList: List[SomethingPlain],
  nestedList: List[List[String]],
  nestedComplexList: List[List[SomethingPlain]],
  option: Option[Int]
)
object SomethingComplex extends HasGenCodec[SomethingComplex] {
  val sthListGen: Gen[List[SomethingPlain]] = SomethingPlain.sizedListOf(8, SomethingPlain.gen)

  val gen: Gen[SomethingComplex] = for {
    embeddedObject <- SomethingPlain.gen
    complexList <- sthListGen
    nestedList <- SomethingPlain.sizedListOf(5, SomethingPlain.stringListGen)
    nestedComplexList <- SomethingPlain.sizedListOf(5, sthListGen)
    option <- arbitrary[Option[Int]]
  } yield SomethingComplex(
    embeddedObject,
    complexList,
    nestedList,
    nestedComplexList,
    option
  )
}

case class SomethingLong(value: Long)
object SomethingLong extends HasGenCodec[SomethingLong]
