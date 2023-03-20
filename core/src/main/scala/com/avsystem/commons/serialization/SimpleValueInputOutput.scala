package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.misc.Unboxing
import com.avsystem.commons.serialization.GenCodec.{ReadFailure, WriteFailure}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object SimpleValueOutput {
  def write[T: GenCodec](value: T): Any = {
    var res = NOpt.empty[Any]
    GenCodec.write[T](new SimpleValueOutput(raw => res = NOpt.some(raw)), value)
    res.getOrElse(throw new WriteFailure("no value was written"))
  }
}

/**
  * An [[Output]] for [[GenCodec]] which serializes data into plain Scala objects.
  *
  * - "lists" are represented as Scala `List`s
  * - "objects" are represented as `String`-keyed Scala `Map`s
  * - simple values (strings, numbers, booleans, byte arrays) are represented as themselves, unchanged
  *
  * In other words, serialized value yield by `SimpleValueOutput` is a Scala object guaranteed to be one of:
  * - `null`
  * - `Int`
  * - `Long`
  * - `Double`
  * - `BigInt`
  * - `BigDecimal`
  * - `Boolean`
  * - `String`
  * - `Array[Byte]`
  * - `scala.collection.Seq[Any]` where every element is also one of the listed types
  * - `scala.collection.Map[String,Any]` where every value is also one of the listed types
  *
  * Such format is often useful as an intermediate representation. For example, it can be later safely passed to
  * standard Java serialization. However, for performance reasons it's recommended to implement dedicated
  * [[Input]] and [[Output]] for the final format (e.g. binary or JSON).
  *
  * @param consumer consumer of serialized value, which is guaranteed to meet the above rules
  */
class SimpleValueOutput(
  consumer: Any => Unit,
  newObjectRepr: => mutable.Builder[(String, Any), BMap[String, Any]],
  newListRepr: => mutable.Builder[Any, BSeq[Any]]
) extends OutputAndSimpleOutput {

  def this(consumer: Any => Unit) =
    this(consumer, new MHashMap[String, Any], new ListBuffer[Any])

  def writeNull(): Unit = consumer(null)
  def writeBoolean(boolean: Boolean): Unit = consumer(boolean)
  def writeString(str: String): Unit = consumer(str)
  def writeInt(int: Int): Unit = consumer(int)
  def writeLong(long: Long): Unit = consumer(long)
  def writeDouble(double: Double): Unit = consumer(double)
  def writeBigInt(bigInt: BigInt): Unit = consumer(bigInt)
  def writeBigDecimal(bigDecimal: BigDecimal): Unit = consumer(bigDecimal)
  def writeBinary(binary: Array[Byte]): Unit = consumer(binary)

  def writeList(): ListOutput = new ListOutput {
    private val buffer = newListRepr
    override def declareSize(size: Int): Unit = buffer.sizeHint(size)
    def writeElement() = new SimpleValueOutput(buffer += _, newObjectRepr, newListRepr)
    def finish(): Unit = consumer(buffer.result())
  }

  def writeObject(): ObjectOutput = new ObjectOutput {
    private val result = newObjectRepr
    override def declareSize(size: Int): Unit = result.sizeHint(size)
    def writeField(key: String) = new SimpleValueOutput(v => result += ((key, v)), newObjectRepr, newListRepr)
    def finish(): Unit = consumer(result)
  }
}

object SimpleValueInput {
  @explicitGenerics def read[T: GenCodec](raw: Any): T =
    GenCodec.read[T](new SimpleValueInput(raw))
}

/**
  * An [[Input]] for [[GenCodec]] complementary to [[SimpleValueOutput]].
  *
  * @param value serialized value yield by [[SimpleValueOutput]]
  */
class SimpleValueInput(value: Any) extends InputAndSimpleInput {
  private def doRead[A >: Null <: AnyRef : ClassTag]: A =
    doReadUnboxed[A, A]

  private def doReadUnboxed[A, B: ClassTag](implicit unboxing: Unboxing[A, B]): A = value match {
    case b: B => unboxing.fun(b)
    case _ => throw new ReadFailure(s"Expected ${classTag[B].runtimeClass} but got ${value.getClass}")
  }

  def readNull(): Boolean = value == null
  def readBoolean(): Boolean = doReadUnboxed[Boolean, JBoolean]
  def readString(): String = doRead[String]
  def readInt(): Int = doReadUnboxed[Int, JInteger]
  def readLong(): Long = doReadUnboxed[Long, JLong]
  def readDouble(): Double = doReadUnboxed[Double, JDouble]
  def readBigInt(): BigInt = doRead[JBigInteger]
  def readBigDecimal(): BigDecimal = doRead[JBigDecimal]
  def readBinary(): Array[Byte] = doRead[Array[Byte]]

  def readObject(): ObjectInput =
    new ObjectInput {
      private val map = doRead[BMap[String, Any]]
      private val it = map.iterator.map {
        case (k, v) => new SimpleValueFieldInput(k, v)
      }
      override def knownSize: Int = if(map.isEmpty) 0 else map.knownSize
      def nextField(): SimpleValueFieldInput = it.next()
      override def peekField(name: String): Opt[SimpleValueFieldInput] =
        map.get(name).map(new SimpleValueFieldInput(name, _)).toOpt // values may be null!
      def hasNext: Boolean = it.hasNext
    }

  def readList(): ListInput =
    new ListInput {
      private val inputSeq: BSeq[Any] = doRead[BSeq[Any]]
      private val it = inputSeq.iterator.map(new SimpleValueInput(_))
      override def knownSize: Int = if(inputSeq.isEmpty) 0 else inputSeq.knownSize
      def nextElement(): SimpleValueInput = it.next()
      def hasNext: Boolean = it.hasNext
    }

  def skip(): Unit = ()
}

class SimpleValueFieldInput(val fieldName: String, value: Any)
  extends SimpleValueInput(value) with FieldInput
