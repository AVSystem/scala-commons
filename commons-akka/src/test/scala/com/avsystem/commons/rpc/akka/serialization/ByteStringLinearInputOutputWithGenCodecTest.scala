package com.avsystem.commons
package rpc.akka.serialization

import akka.util.ByteString
import com.avsystem.commons.rpc.akka.{InnerRPC, TestRPC}
import com.avsystem.commons.serialization.GenCodec
import org.scalatest.{FlatSpec, Inside, Matchers}

/**
  * Test assumes that GenCodec materialization works correctly for proper types
  *
  * @author Wojciech Milewski
  */
class ByteStringLinearInputOutputWithGenCodecTest extends FlatSpec with Matchers with Inside {

  trait Fixture {
    val output = new ByteStringLinearOutput(ByteString.newBuilder)

    def input = new ByteStringLinearInput(output.result)
  }

  it should "serialize null" in new Fixture {
    GenCodec.NullCodec.write(output, null)

    noException should be thrownBy GenCodec.NullCodec.read(input)
  }

  it should "serialize unit" in new Fixture {
    GenCodec.UnitCodec.write(output, Unit)

    noException should be thrownBy GenCodec.UnitCodec.read(input)
  }

  it should "serialize empty string" in new Fixture {
    GenCodec.StringCodec.write(output, "")

    val result = GenCodec.StringCodec.read(input)
    result shouldBe ""
  }

  it should "serialize nonempty string" in new Fixture {
    val string = "Don't read this text"
    GenCodec.StringCodec.write(output, string)

    GenCodec.StringCodec.read(input) shouldBe string
  }

  it should "serialize character" in new Fixture {
    val char = 'c'
    GenCodec.CharCodec.write(output, char)

    GenCodec.CharCodec.read(input) shouldBe char
  }

  it should "serialize true boolean" in new Fixture {
    GenCodec.BooleanCodec.write(output, true)

    GenCodec.BooleanCodec.read(input) shouldBe true
  }

  it should "serialize false boolean" in new Fixture {
    GenCodec.BooleanCodec.write(output, false)

    GenCodec.BooleanCodec.read(input) shouldBe false
  }

  it should "serialize byte" in new Fixture {
    val byte = 42.toByte
    GenCodec.ByteCodec.write(output, byte)

    GenCodec.ByteCodec.read(input) shouldBe byte
  }

  it should "serialize short" in new Fixture {
    val short = 42.toShort
    GenCodec.ShortCodec.write(output, short)

    GenCodec.ShortCodec.read(input) shouldBe short
  }

  it should "serialize int" in new Fixture {
    val int = 42
    GenCodec.IntCodec.write(output, int)

    GenCodec.IntCodec.read(input) shouldBe int
  }

  it should "serialize long" in new Fixture {
    val long = 42.toLong
    GenCodec.LongCodec.write(output, long)

    GenCodec.LongCodec.read(input) shouldBe long
  }

  it should "serialize float" in new Fixture {
    val float = 42.42f
    GenCodec.FloatCodec.write(output, float)

    GenCodec.FloatCodec.read(input) shouldBe float
  }

  it should "serialize double" in new Fixture {
    val double = 42.42
    GenCodec.DoubleCodec.write(output, double)

    GenCodec.DoubleCodec.read(input) shouldBe double
  }

  it should "serialize binary array" in new Fixture {
    val bytes = Array.range(0, 10).map(_.toByte)
    GenCodec.ByteArrayCodec.write(output, bytes)

    GenCodec.ByteArrayCodec.read(input) shouldBe bytes
  }

  it should "serialize list" in new Fixture {
    GenCodec.write(output, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    GenCodec.read[List[Int]](input) should contain inOrderOnly(1, 2, 3, 4, 5)
  }

  it should "serialize nested lists" in new Fixture {

    val firstInner = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    val secondInner = 2 :: 3 :: 4 :: Nil

    val outer = firstInner :: secondInner :: Nil

    GenCodec.write(output, outer)

    private val result = GenCodec.read[List[List[Int]]](input)
    val firstResult :: secondResult :: Nil = result

    firstResult should contain inOrderOnly(1, 2, 3, 4, 5)
    secondResult should contain inOrderOnly(2, 3, 4)

  }

  it should "serialize object" in new Fixture {

    val o = Nested("str", null, boolean = true, 42, 1 :: 2 :: 3 :: 4 :: Nil)

    GenCodec.write(output, o)

    GenCodec.read[Nested](input) shouldBe o
  }

  it should "serialize nested object" in new Fixture {

    val nested = Nested("str", null, boolean = true, 42, 1 :: 2 :: 3 :: 4 :: Nil)
    val o = Something("lol", null, boolean = false, 69, 2 :: 3 :: 4 :: Nil, nested)

    GenCodec.write(output, o)

    GenCodec.read[Something](input) shouldBe o

  }

}
