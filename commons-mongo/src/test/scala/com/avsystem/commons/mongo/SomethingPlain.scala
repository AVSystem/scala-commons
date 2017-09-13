package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec

import scala.util.Random
import scala.util.hashing.MurmurHash3

object Randoms {
  def randBoolean: Boolean = Random.nextBoolean()
  def randInt: Int = Random.nextInt(1024)
  def randLong: Long = Random.nextLong()
  def randDouble: Double = Random.nextDouble()
  def randString: String = Random.nextString(4)
  def randBytes: BytesWrapper = {
    val array = new Array[Byte](128)
    Random.nextBytes(array)
    new BytesWrapper(array)
  }
  def randList[T](generator: => T): List[T] = List.fill(Random.nextInt(4))(generator)
  def randListNonEmpty[T](generator: => T): List[T] = List.fill(Random.nextInt(4) + 1)(generator)
  def randMap[T](valueGenerator: => T): IMap[String, T] = {
    val b = Map.newBuilder[String, T]
    for (i <- 0 until Random.nextInt(4)) {
      b += s"${randString}_$i" -> valueGenerator
    }
    b.result()
  }
}

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

  import Randoms._

  implicitly[GenCodec[JDate]]
  implicit val codec: GenCodec[SomethingPlain] = GenCodec.materialize

  def random = SomethingPlain(
    string = randString,
    boolean = randBoolean,
    int = randInt,
    long = randLong,
    timestamp = new JDate(),
    double = randDouble,
    binary = randBytes,
    list = randList(randString),
    map = randMap(randString)
  )
}

case class SomethingComplex(
  embeddedObject: SomethingPlain,
  complexList: List[SomethingPlain],
  nestedList: List[List[String]],
  nestedComplexList: List[List[SomethingPlain]]
)
object SomethingComplex {

  import Randoms._

  implicit val codec: GenCodec[SomethingComplex] = GenCodec.materialize

  def random = SomethingComplex(
    embeddedObject = SomethingPlain.random,
    complexList = randListNonEmpty(SomethingPlain.random),
    nestedList = randListNonEmpty(randListNonEmpty(randString)),
    nestedComplexList = randListNonEmpty(randListNonEmpty(SomethingPlain.random))
  )
}
