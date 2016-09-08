package com.avsystem.commons
package serialization

import com.avsystem.commons.jiop.BasicJavaInterop._
import com.avsystem.commons.misc.Unboxing
import com.avsystem.commons.serialization.GenCodec.ReadFailure

import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}

class SimpleValueInput(value: Any) extends Input {
  private def doRead[A: ClassTag]: A =
    doReadUnboxed[A, A](classTag[A], Unboxing(identity))

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
    private val builder = Map.newBuilder[String, Any]
    def writeField(key: String) = new SimpleValueOutput(v => builder += ((key, v)))
    def finish() = consumer(builder.result())
  }

  def writeLong(long: Long) = consumer(long)
  def writeNull() = consumer(null)
}
