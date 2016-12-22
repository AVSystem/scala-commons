package com.avsystem.commons
package serialization

import com.avsystem.commons.jiop.BasicJavaInterop._
import com.avsystem.commons.misc.Unboxing
import com.avsystem.commons.serialization.GenCodec.ReadFailure

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}

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
class SimpleValueOutput(consumer: Any => Unit) extends Output {
  def writeBinary(binary: Array[Byte]) = consumer(binary)
  def writeString(str: String) = consumer(str)
  def writeDouble(double: Double) = consumer(double)
  def writeInt(int: Int) = consumer(int)

  def writeList() = new ListOutput {
    private val buffer = new ListBuffer[Any]
    def writeElement() = new SimpleValueOutput(buffer += _)
    def finish() = consumer(buffer.result())
  }

  def writeBoolean(boolean: Boolean) = consumer(boolean)

  def writeObject() = new ObjectOutput {
    private val result = new mutable.LinkedHashMap[String, Any]
    def writeField(key: String) = new SimpleValueOutput(v => result += ((key, v)))
    def finish() = consumer(result)
  }

  def writeLong(long: Long) = consumer(long)
  def writeNull() = consumer(null)
}

/**
  * An [[Input]] for [[GenCodec]] complementary to [[SimpleValueOutput]].
  *
  * @param value serialized value yield by [[SimpleValueOutput]]
  */
class SimpleValueInput(value: Any) extends Input {
  private def doRead[A >: Null <: AnyRef : ClassTag]: A =
    doReadUnboxed[A, A]

  private def doReadUnboxed[A, B: ClassTag](implicit unboxing: Unboxing[A, B]): A = value match {
    case b: B => unboxing.fun(b)
    case _ => throw new ReadFailure(s"Expected ${classTag[B].runtimeClass} but got ${value.getClass}")
  }

  def inputType = value match {
    case null => InputType.Null
    case _: List[Any] => InputType.List
    case _: Map[_, Any] => InputType.Object
    case _ => InputType.Simple
  }

  def readBinary() = doRead[Array[Byte]]
  def readLong() = doReadUnboxed[Long, JLong]
  def readNull() = if (value == null) null else throw new ReadFailure("not null")
  def readObject() =
    new ObjectInput {
      private val it = doRead[Map[String, Any]].iterator.map {
        case (k, v) => new SimpleValueFieldInput(k, v)
      }
      def nextField() = it.next()
      def hasNext = it.hasNext
    }

  def readInt() = doReadUnboxed[Int, JInteger]
  def readString() = doRead[String]

  def readList() =
    new ListInput {
      private val it = doRead[List[Any]].iterator.map(new SimpleValueInput(_))
      def nextElement() = it.next()
      def hasNext = it.hasNext
    }

  def readBoolean() = doReadUnboxed[Boolean, JBoolean]
  def readDouble() = doReadUnboxed[Double, JDouble]

  def skip() = ()
}

class SimpleValueFieldInput(val fieldName: String, value: Any) extends SimpleValueInput(value) with FieldInput
