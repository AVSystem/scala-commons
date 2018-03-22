package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.HasGenCodec

import scala.util.Random

trait SerializationTestUtils {
  case class TestCC(i: Int, l: Long, intAsDouble: Double, b: Boolean, s: String, list: List[Char])
  object TestCC extends HasGenCodec[TestCC]

  case class NestedTestCC(i: Int, t: TestCC, t2: TestCC)
  object NestedTestCC extends HasGenCodec[NestedTestCC]

  case class DeepNestedTestCC(n: NestedTestCC, l: DeepNestedTestCC)
  object DeepNestedTestCC extends HasGenCodec[DeepNestedTestCC]

  case class CompleteItem(
    unit: Unit, string: String, specialString: String, char: Char,
    boolean: Boolean, byte: Byte, short: Short, int: Int,
    long: Long, float: Float, double: Double, binary: Array[Byte], list: List[String],
    set: Set[String], obj: TestCC, map: Map[String, Int]
  )
  object CompleteItem extends HasGenCodec[CompleteItem]

  def completeItem() = CompleteItem(
    unit = (),
    string = Random.nextString(Random.nextInt(20)),
    specialString = "\n\f\b\t\r\n\\\"\\\\",
    char = Random.nextString(1).head,
    boolean = Random.nextBoolean(),
    byte = Random.nextInt().toByte,
    short = Random.nextInt().toShort,
    int = Random.nextInt(),
    long = Random.nextLong(),
    float = Random.nextFloat(),
    double = Random.nextDouble(),
    binary = Array.fill(Random.nextInt(20))(Random.nextInt().toByte),
    list = List.fill(Random.nextInt(20))(Random.nextString(Random.nextInt(20))),
    set = List.fill(Random.nextInt(20))(Random.nextString(Random.nextInt(20))).toSet,
    obj = TestCC(Random.nextInt(), Random.nextLong(), Random.nextInt(), Random.nextBoolean(), Random.nextString(Random.nextInt(20)), Nil),
    map = Map(Seq.fill(Random.nextInt(20))(Random.nextString(20) -> Random.nextInt()): _*)
  )

}
