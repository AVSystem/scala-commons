package com.avsystem.commons
package rpc.akka.serialization

import akka.util.ByteString
import org.scalatest.{FlatSpec, Inside, Matchers}

/**
  * Test assumes that GenCodec materialization works correctly for proper types
  *
  * @author Wojciech Milewski
  */
class ByteStringLinearInputOutputTest extends FlatSpec with Matchers with Inside {

  trait Fixture {
    val output = new ByteStringLinearOutput(ByteString.newBuilder)

    def input = new ByteStringLinearInput(output.result)
  }

  it should "serialize null" in new Fixture {
    output.writeNull()

    noException should be thrownBy input.readNull()
  }

  it should "serialize unit" in new Fixture {
    output.writeUnit()

    noException should be thrownBy input.readUnit()
  }

  it should "serialize empty string" in new Fixture {
    output.writeString("")

    val result = input.readString()
    result shouldBe ""
  }

  it should "serialize nonempty string" in new Fixture {
    val string = "Don't read this text"

    output.writeString(string)

    input.readString() shouldBe string
  }

  it should "serialize character" in new Fixture {
    val char = 'c'
    output.writeChar(char)

    input.readChar() shouldBe char
  }

  it should "serialize true boolean" in new Fixture {
    output.writeBoolean(true)

    input.readBoolean() shouldBe true
  }

  it should "serialize false boolean" in new Fixture {
    output.writeBoolean(false)

    input.readBoolean() shouldBe false
  }

  it should "serialize byte" in new Fixture {
    val byte = 42.toByte
    output.writeByte(byte)

    input.readByte() shouldBe byte
  }

  it should "serialize short" in new Fixture {
    val short = 42.toShort
    output.writeShort(short)

    input.readShort() shouldBe short
  }

  it should "serialize int" in new Fixture {
    val int = 42
    output.writeInt(int)

    input.readInt() shouldBe int
  }

  it should "serialize long" in new Fixture {
    val long = 42.toLong
    output.writeLong(long)

    input.readLong() shouldBe long
  }

  it should "serialize float" in new Fixture {
    val float = 42.42f
    output.writeFloat(float)

    input.readFloat() shouldBe float
  }

  it should "serialize double" in new Fixture {
    val double = 42.42
    output.writeDouble(double)

    input.readDouble() shouldBe double
  }

  it should "serialize binary array" in new Fixture {
    val bytes = Array.range(0, 10).map(_.toByte)
    output.writeBinary(bytes)

    input.readBinary() shouldBe bytes
  }

  it should "serialize list" in new Fixture {
    val list = output.writeList()
    1.to(5).foreach { i =>
      list.writeElement().writeInt(i)
    }
    list.finish()

    val listInput = input.readList()
    listInput.iterator(_.readInt()).toStream should contain inOrderOnly(1, 2, 3, 4, 5)
  }

  it should "serialize nested lists" in new Fixture {
    val list = output.writeList()

    val firstList = list.writeElement().writeList()
    1.to(5).foreach { i =>
      firstList.writeElement().writeInt(i)
    }
    firstList.finish()
    val secondList = list.writeElement().writeList()
    "abcdef".toCharArray.foreach(c => secondList.writeElement().writeChar(c))
    secondList.finish()
    list.finish()

    val listInput = input.readList()
    val firstListInput = listInput.nextElement().readList()
    firstListInput.iterator(_.readInt()).toStream should contain inOrderOnly(1, 2, 3, 4, 5)
    val secondListInput = listInput.nextElement().readList()
    secondListInput.iterator(_.readChar()).toStream should contain inOrderOnly('a', 'b', 'c', 'd', 'e', 'f')
  }

  it should "serialize object" in new Fixture {
    val objectOutput = output.writeObject()
    objectOutput.writeField("first").writeInt(1)
    objectOutput.writeField("second").writeNull()
    objectOutput.writeField("third").writeString("3")
    objectOutput.finish()

    val objectInput = input.readObject()
    inside(objectInput.nextField()) {
      case input if input.fieldName == "first" => input.readInt() shouldBe 1
    }
    inside(objectInput.nextField()) {
      case input if input.fieldName == "second" => noException should be thrownBy input.readNull()
    }
    inside(objectInput.nextField()) {
      case input if input.fieldName == "third" => input.readString() shouldBe "3"
    }
    objectInput.hasNext shouldBe false
  }

  it should "serialize nested object" in new Fixture {
    val objectOutput = output.writeObject()
    objectOutput.writeField("first").writeString("lol")
    val nestedObject = objectOutput.writeField("object").writeObject()
    nestedObject.writeField("first").writeInt(1)
    nestedObject.writeField("second").writeInt(2)
    val nestedList = nestedObject.writeField("nestedList").writeList()
    1.to(5).foreach { i =>
      nestedList.writeElement().writeInt(i)
    }
    nestedList.finish()
    nestedObject.writeField("third").writeUnit()
    nestedObject.finish()
    objectOutput.writeField("second").writeNull()
    objectOutput.writeField("last").writeInt(3)
    objectOutput.finish()

    val objectInput = input.readObject()
    objectInput.hasNext shouldBe true
    inside(objectInput.nextField()) {
      case input if input.fieldName == "first" => input.readString() shouldBe "lol"
    }
    objectInput.hasNext shouldBe true
    inside(objectInput.nextField()) {
      case input if input.fieldName == "object" =>
        val nestedInput = input.readObject()
        nestedInput.hasNext shouldBe true
        inside(nestedInput.nextField()) {
          case i if i.fieldName == "first" => i.readInt() shouldBe 1
        }
        nestedInput.hasNext shouldBe true
        inside(nestedInput.nextField()) {
          case i if i.fieldName == "second" => i.readInt() shouldBe 2
        }
        nestedInput.hasNext shouldBe true
        inside(nestedInput.nextField()) {
          case i if i.fieldName == "nestedList" => i.readList().iterator(_.readInt()).toStream should contain inOrderOnly(1, 2, 3, 4, 5)
        }
        nestedInput.hasNext shouldBe true
        inside(nestedInput.nextField()) {
          case i if i.fieldName == "third" => noException should be thrownBy i.readUnit()
        }
        nestedInput.hasNext shouldBe false
    }
    objectInput.hasNext shouldBe true
    inside(objectInput.nextField()) {
      case input if input.fieldName == "second" => noException should be thrownBy input.readNull()
    }
    objectInput.hasNext shouldBe true
    inside(objectInput.nextField()) {
      case input if input.fieldName == "last" => input.readInt() shouldBe 3
    }
    objectInput.hasNext shouldBe false
  }

  it should "skip null correctly" in new Fixture {
    private val objectOutput = output.writeObject()
    objectOutput.writeField("first").writeNull()
    objectOutput.writeField("second").writeLong(5L)
    objectOutput.finish()

    private val readObject = input.readObject()
    readObject.hasNext shouldBe true
    inside(readObject.nextField()) {
      case input if input.fieldName == "first" => input.skip()
    }
    readObject.hasNext shouldBe true
    inside(readObject.nextField()) {
      case input if input.fieldName == "second" => input.readLong() shouldBe 5L
    }
    readObject.hasNext shouldBe false
  }

}
