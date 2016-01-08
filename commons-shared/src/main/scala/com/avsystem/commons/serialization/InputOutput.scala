package com.avsystem.commons
package serialization

import com.avsystem.commons.collection.CollectionAliases.BMap

trait Output {
  def writeNull(): Unit
  def writeUnit(): Unit = writeNull()
  def writeString(str: String): Unit
  def writeChar(char: Char): Unit = writeString(char.toString)
  def writeBoolean(boolean: Boolean): Unit
  def writeByte(byte: Byte): Unit = writeShort(byte)
  def writeShort(short: Short): Unit = writeInt(short)
  def writeInt(int: Int): Unit
  def writeLong(long: Long): Unit
  def writeTimestamp(millis: Long): Unit = writeLong(millis)
  def writeFloat(float: Float): Unit = writeDouble(float)
  def writeDouble(double: Double): Unit
  def writeBinary(binary: Array[Byte]): Unit
  def writeList(): ListOutput
  def writeSet(): ListOutput = writeList()
  def writeObject(): ObjectOutput
  def writeMap(): ObjectOutput = writeObject()
}
trait SequentialOutput {
  def finish(): Unit
}
trait ListOutput extends SequentialOutput {
  def writeElement(): Output
}
trait ObjectOutput extends SequentialOutput {
  def writeField(key: String): Output
}

trait Input {
  def readNull(): ValueRead[Null]
  def readUnit(): ValueRead[Unit] = readNull().map(_ => ())
  def readString(): ValueRead[String]
  def readChar(): ValueRead[Char] = readString().map(_.charAt(0))
  def readBoolean(): ValueRead[Boolean]
  def readByte(): ValueRead[Byte] = readShort().map(_.toByte)
  def readShort(): ValueRead[Short] = readInt().map(_.toShort)
  def readInt(): ValueRead[Int]
  def readLong(): ValueRead[Long]
  def readTimestamp(): ValueRead[Long] = readLong()
  def readFloat(): ValueRead[Float] = readDouble().map(_.toFloat)
  def readDouble(): ValueRead[Double]
  def readBinary(): ValueRead[Array[Byte]]
  def readList(): ValueRead[ListInput]
  def readSet(): ValueRead[ListInput] = readList()
  def readObject(): ValueRead[ObjectInput]
  def readMap(): ValueRead[ObjectInput] = readObject()
  def skip(): Unit
}
trait SequentialInput {
  def hasNext: Boolean
  def skipAll(): Unit
}
trait ListInput extends SequentialInput {self =>
  def nextElement(): Input

  def skipAll() = while (hasNext) nextElement().skip()
  def iterator[A](readFun: Input => A): Iterator[A] =
    new Iterator[A] {
      def hasNext = self.hasNext
      def next() = readFun(nextElement())
    }
}
trait ObjectInput extends SequentialInput {self =>
  def nextField(): (String, Input)

  def skipAll() = while (hasNext) nextField()._2.skip()
  def iterator[A](readFun: Input => A): Iterator[(String, A)] =
    new Iterator[(String, A)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, i) = nextField()
        (k, readFun(i))
      }
    }
}
