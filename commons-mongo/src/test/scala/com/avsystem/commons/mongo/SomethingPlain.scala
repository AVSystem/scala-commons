package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import _root_.scala.util.hashing.MurmurHash3

class BytesWrapper(val bytes: Array[Byte]) {
  override def hashCode(): Int = MurmurHash3.bytesHash(bytes)
  override def equals(other: Any): Boolean = other match {
    case b: BytesWrapper => java.util.Arrays.equals(bytes, b.bytes)
    case _ => false
  }
}
object BytesWrapper {
  implicit val codec: GenCodec[BytesWrapper] = GenCodec.createNullSafe(
    input => new BytesWrapper(input.readBinary()),
    (output, bytes) => output.writeBinary(bytes.bytes),
    allowNull = true
  )
}

case class SomethingPlain(
  string: String,
  boolean: Boolean,
  int: Int,
  long: Long,
  timestamp: JDate,
  double: Double,
  binary: BytesWrapper,
  list: List[String],
  map: Map[String, String]
)
object SomethingPlain {
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
    long <- arbitrary[Long]
    timestamp <- arbitrary[JDate]
    double <- arbitrary[Double]
    binary <- Gen.buildableOf[Array[Byte], Byte](arbitrary[Byte]).map(new BytesWrapper(_))
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

  implicit val codec: GenCodec[SomethingPlain] = GenCodec.materialize
}

case class SomethingComplex(
  embeddedObject: SomethingPlain,
  complexList: List[SomethingPlain],
  nestedList: List[List[String]],
  nestedComplexList: List[List[SomethingPlain]]
)
object SomethingComplex {
  val sthListGen: Gen[List[SomethingPlain]] = SomethingPlain.sizedListOf(8, SomethingPlain.gen)

  val gen: Gen[SomethingComplex] = for {
    embeddedObject <- SomethingPlain.gen
    complexList <- sthListGen
    nestedList <- SomethingPlain.sizedListOf(5, SomethingPlain.stringListGen)
    nestedComplexList <- SomethingPlain.sizedListOf(5, sthListGen)
  } yield SomethingComplex(
    embeddedObject,
    complexList,
    nestedList,
    nestedComplexList
  )

  implicit val codec: GenCodec[SomethingComplex] = GenCodec.materialize
}
