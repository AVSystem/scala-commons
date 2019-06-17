package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.redis.protocol.BulkStringMsg
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.{JsonReader, JsonStringInput, JsonStringOutput}

import scala.collection.mutable

/**
  * Typeclass which expresses that values of some type are serializable to binary form (`ByteString`) and deserializable
  * from it in order to use them as keys, hash keys and values in Redis commands.
  *
  * By default, `RedisDataCodec` is provided for simple types like `String`, `ByteString`, `Array[Byte]`,
  * `Boolean`, `Char`, all primitive numeric types and `NamedEnum`s
  * (which have `NamedEnumCompanion`).
  *
  * Also, all types which have an instance of `GenCodec`
  * automatically have an instance of RedisDataCodec.
  */
case class RedisDataCodec[T](read: ByteString => T, write: T => ByteString)
object RedisDataCodec extends LowPriorityRedisDataCodecs {
  def apply[T](implicit rdc: RedisDataCodec[T]): RedisDataCodec[T] = rdc

  def write[T](value: T)(implicit rdc: RedisDataCodec[T]): ByteString = rdc.write(value)
  def read[T](raw: ByteString)(implicit rdc: RedisDataCodec[T]): T = rdc.read(raw)

  implicit val ByteStringCodec: RedisDataCodec[ByteString] =
    RedisDataCodec(identity, identity)
  implicit val ByteArrayCodec: RedisDataCodec[Array[Byte]] =
    RedisDataCodec(_.toArray, ByteString(_))
}
trait LowPriorityRedisDataCodecs { this: RedisDataCodec.type =>
  implicit def fromGenCodec[T: GenCodec]: RedisDataCodec[T] =
    RedisDataCodec(bytes => RedisDataInput.read(bytes), value => RedisDataOutput.write(value))
}

object RedisDataUtils {
  final val Null = ByteString(0)
}

object RedisDataOutput {
  def write[T: GenCodec](value: T): ByteString = {
    var bs: ByteString = null
    GenCodec.write(new RedisDataOutput(bs = _), value)
    bs
  }
}

final class RedisDataOutput(consumer: ByteString => Unit) extends OutputAndSimpleOutput {
  private def writeBytes(bytes: ByteString): Unit =
    if (bytes.headOpt.contains(0: Byte)) consumer(RedisDataUtils.Null ++ bytes)
    else consumer(bytes)

  def writeNull(): Unit = consumer(RedisDataUtils.Null)
  def writeBoolean(boolean: Boolean): Unit = writeInt(if (boolean) 1 else 0)
  def writeString(str: String): Unit = writeBytes(ByteString(str))
  def writeInt(int: Int): Unit = writeString(int.toString)
  def writeLong(long: Long): Unit = writeString(long.toString)
  def writeDouble(double: Double): Unit = writeString(double.toString)
  def writeBigInt(bigInt: BigInt): Unit = writeString(bigInt.toString)
  def writeBigDecimal(bigDecimal: BigDecimal): Unit = writeString(bigDecimal.toString)
  def writeBinary(binary: Array[Byte]): Unit = writeBytes(ByteString(binary))

  def writeList(): ListOutput = new ListOutput {
    private val sb = new JStringBuilder
    private val jlo = new JsonStringOutput(sb).writeList()

    def writeElement(): Output = jlo.writeElement()
    def finish(): Unit = {
      jlo.finish()
      consumer(ByteString(sb.toString))
    }
  }

  def writeObject(): ObjectOutput = new ObjectOutput {
    private val sb = new JStringBuilder
    private val joo = new JsonStringOutput(sb).writeObject()

    def writeField(key: String): Output = joo.writeField(key)
    def finish(): Unit = {
      joo.finish()
      consumer(ByteString(sb.toString))
    }
  }
}

class RedisRecordOutput(builder: mutable.Growable[BulkStringMsg]) extends ObjectOutput {
  def writeField(key: String): Output = {
    builder += BulkStringMsg(ByteString(key))
    new RedisDataOutput(bs => builder += BulkStringMsg(bs))
  }

  def finish(): Unit = ()
}

object RedisDataInput {
  def read[T: GenCodec](bytes: ByteString): T =
    GenCodec.read[T](new RedisDataInput(bytes))
}

class RedisDataInput(bytes: ByteString) extends InputAndSimpleInput {
  private lazy val jsonInput = new JsonStringInput(new JsonReader(readBytes().utf8String))

  private def fail(msg: String) = throw new ReadFailure(msg)
  private def readBytes(): ByteString = bytes match {
    case RedisDataUtils.Null => fail("null")
    case _ if bytes.headOpt.contains(0: Byte) => bytes.drop(1)
    case _ => bytes
  }

  def readNull(): Boolean = bytes == RedisDataUtils.Null
  def readString(): String = readBytes().utf8String
  def readBoolean(): Boolean = readString().toInt > 0
  def readInt(): Int = readString().toInt
  def readLong(): Long = readString().toLong
  def readDouble(): Double = readString().toDouble
  def readBigInt(): BigInt = BigInt(readString())
  def readBigDecimal(): BigDecimal = BigDecimal(readString())
  def readBinary(): Array[Byte] = readBytes().toArray

  def readList(): ListInput = jsonInput.readList()
  def readObject(): ObjectInput = jsonInput.readObject()

  def skip(): Unit = ()
}

class RedisFieldDataInput(val fieldName: String, bytes: ByteString)
  extends RedisDataInput(bytes) with FieldInput

class RedisRecordInput(bulks: IndexedSeq[BulkStringMsg]) extends ObjectInput {
  private val it = bulks.iterator.map(_.string)

  def nextField(): FieldInput = {
    val fieldName = it.next.utf8String
    val bytes = it.next
    new RedisFieldDataInput(fieldName, bytes)
  }

  def hasNext: Boolean = it.hasNext
}
