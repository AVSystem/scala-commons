package com.avsystem.commons
package rpc.akka.serialization

import org.scalatest.{FlatSpec, Inside, Matchers}

/**
  * Test assumes that GenCodec materialization works correctly for proper types
  *
  * @author Wojciech Milewski
  */
class ByteStringInputOutputTest extends FlatSpec with Matchers with Inside {

  trait Fixture {
    val output = new ByteStringOutput

    def input = new ByteStringInput(output.result)
  }

  it should "serialize null" in new Fixture {
    output.writeNull()

    noException should be thrownBy input.readNull().get
  }

  it should "serialize unit" in new Fixture {
    output.writeUnit()

    noException should be thrownBy input.readUnit().get
  }

  it should "serialize empty string" in new Fixture {
    output.writeString("")

    val result = input.readString().get
    result shouldBe ""
  }

  it should "serialize nonempty string" in new Fixture {
    val string = "Don't read this text"

    output.writeString(string)

    input.readString().get shouldBe string
  }

  it should "serialize character" in new Fixture {
    val char = 'c'
    output.writeChar(char)

    input.readChar().get shouldBe char
  }

  it should "serialize true boolean" in new Fixture {
    output.writeBoolean(true)

    input.readBoolean().get shouldBe true
  }

  it should "serialize false boolean" in new Fixture {
    output.writeBoolean(false)

    input.readBoolean().get shouldBe false
  }

  it should "serialize byte" in new Fixture {
    val byte = 42.toByte
    output.writeByte(byte)

    input.readByte().get shouldBe byte
  }

  it should "serialize short" in new Fixture {
    val short = 42.toShort
    output.writeShort(short)

    input.readShort().get shouldBe short
  }

  it should "serialize int" in new Fixture {
    val int = 42
    output.writeInt(int)

    input.readInt().get shouldBe int
  }

  it should "serialize long" in new Fixture {
    val long = 42.toLong
    output.writeLong(long)

    input.readLong().get shouldBe long
  }

  it should "serialize float" in new Fixture {
    val float = 42.42f
    output.writeFloat(float)

    input.readFloat().get shouldBe float
  }

  it should "serialize double" in new Fixture {
    val double = 42.42
    output.writeDouble(double)

    input.readDouble().get shouldBe double
  }

  it should "serialize binary array" in new Fixture {
    val bytes = Array.range(0, 10).map(_.toByte)
    output.writeBinary(bytes)

    input.readBinary().get shouldBe bytes
  }

  it should "serialize list" in new Fixture {
    val list = output.writeList()
    1.to(5).foreach { i =>
      list.writeElement().writeInt(i)
    }
    list.finish()

    val listInput = input.readList().get
    listInput.iterator(_.readInt().get).toStream should contain inOrderOnly(1, 2, 3, 4, 5)
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

    val listInput = input.readList().get
    val firstListInput = listInput.nextElement().readList().get
    firstListInput.iterator(_.readInt().get).toStream should contain inOrderOnly(1, 2, 3, 4, 5)
    val secondListInput = listInput.nextElement().readList().get
    secondListInput.iterator(_.readChar().get).toStream should contain inOrderOnly('a', 'b', 'c', 'd', 'e', 'f')
  }

  it should "serialize object" in new Fixture {
    val objectOutput = output.writeObject()
    objectOutput.writeField("first").writeInt(1)
    objectOutput.writeField("second").writeNull()
    objectOutput.writeField("third").writeString("3")
    objectOutput.finish()

    val objectInput = input.readObject().get
    inside(objectInput.nextField()) {
      case ("first", input) => input.readInt().get shouldBe 1
    }
    inside(objectInput.nextField()) {
      case ("second", input) => noException should be thrownBy input.readNull().get
    }
    inside(objectInput.nextField()) {
      case ("third", input) => input.readString().get shouldBe "3"
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

    val objectInput = input.readObject().get
    inside(objectInput.nextField()) {
      case ("first", input) => input.readString().get shouldBe "lol"
    }
    inside(objectInput.nextField()) {
      case ("object", input) =>
        val nestedInput = input.readObject().get
        inside(nestedInput.nextField()) {
          case ("first", i) => i.readInt().get shouldBe 1
        }
        inside(nestedInput.nextField()) {
          case ("second", i) => i.readInt().get shouldBe 2
        }
        inside(nestedInput.nextField()) {
          case ("nestedList", i) => i.readList().get.iterator(_.readInt().get).toStream should contain inOrderOnly(1, 2, 3, 4, 5)
        }
        inside(nestedInput.nextField()) {
          case ("third", i) => noException should be thrownBy i.readUnit().get
        }
    }
    inside(objectInput.nextField()) {
      case ("second", input) => noException should be thrownBy input.readNull().get
    }
    inside(objectInput.nextField()) {
      case ("last", input) => input.readInt().get shouldBe 3
    }
  }

  it should "skip null correctly" in new Fixture {
    private val objectOutput = output.writeObject()
    objectOutput.writeField("first").writeNull()
    objectOutput.writeField("second").writeLong(5L)
    objectOutput.finish()

    private val readObject = input.readObject().get
    inside(readObject.nextField()) {
      case ("first", input) => input.skip()
    }
    inside(readObject.nextField()) {
      case ("second", input) => input.readLong().get shouldBe 5L
    }
  }

}
